{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Verismith.Verilog.CodeGen
-- Description : Code generation for Verilog AST.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module generates the code from the Verilog AST defined in
-- "Verismith.Verilog.AST".
module Verismith.Verilog.CodeGen
  ( -- * Code Generation
    GenVerilog (..),
    Source (..),
    render,
  )
where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Numeric (showHex)
import Verismith.Internal hiding (comma)
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec

-- | 'Source' class which determines that source code is able to be generated
-- from the data structure using 'genSource'. This will be stored in 'Text' and
-- can then be processed further.
class Source a where
  genSource :: a -> Text

-- | Map a 'Maybe (Statement ann)' to 'Text'. If it is 'Just statement', the generated
-- statements are returned. If it is 'Nothing', then @;\n@ is returned.
defMap :: Annotation ann => Maybe (Statement ann) -> Doc a
defMap = maybe semi statement

-- | Convert the 'Verilog ann' type to 'Text' so that it can be rendered.
verilogSrc :: Annotation ann => Verilog ann -> Doc a
verilogSrc (Verilog modules) = vsep . punctuate line $ moduleDecl <$> modules

-- | Generate the 'ModDecl ann' for a module and convert it to 'Text'.
moduleDecl :: Annotation ann => ModDecl ann -> Doc a
moduleDecl (ModDecl ann i outP inP items ps) =
  vsep
    [ hsep ["/*", pretty $ show ann, "*/"],
      sep ["module" <+> identifier i, params ps, ports <> semi],
      indent 2 modI,
      "endmodule"
    ]
  where
    ports
      | null outP && null inP = ""
      | otherwise = parens . align . sep . punctuate comma $ modPort <$> outIn
    modI = vsep $ moduleItem <$> items
    outIn = outP ++ inP
    params [] = ""
    params (p : pps) = hcat ["#(", paramList (p :| pps), ")"]

-- | Generates a parameter list. Can only be called with a 'NonEmpty' list.
paramList :: NonEmpty (Parameter ann) -> Doc a
paramList ps = vsep . punctuate ", " . toList $ parameter <$> ps

-- | Generates a localparam list. Can only be called with a 'NonEmpty' list.
localParamList :: NonEmpty (LocalParam ann) -> Doc a
localParamList ps = vsep . punctuate ", " . toList $ localParam <$> ps

-- | Generates the assignment for a 'Parameter'.
parameter :: Parameter ann -> Doc a
parameter (Parameter name val) =
  hsep ["parameter", identifier name, "=", constExpr val]

-- | Generates the assignment for a 'LocalParam'.
localParam :: LocalParam ann -> Doc a
localParam (LocalParam name val) =
  hsep ["localparameter", identifier name, "=", constExpr val]

identifier :: Identifier -> Doc a
identifier (Identifier i) = pretty i

-- | Converts 'Port' to 'Text' for the module list, which means it only
-- generates a list of identifiers.
modPort :: Port ann -> Doc a
modPort (Port _ _ _ i) = identifier i

addOpt :: Bool -> Doc a -> [Doc a] -> [Doc a]
addOpt b a = if b then (a :) else id

addMay :: Maybe (Doc a) -> [Doc a] -> [Doc a]
addMay Nothing = id
addMay (Just a) = (a :)

-- | Generate the 'Port' description.
port :: Port ann -> Doc a
port (Port tp sgn r name) =
  hsep $ pType tp : addOpt sgn "signed" [range r, identifier name]

range :: Range ann -> Doc a
range (Range msb lsb) = brackets $ hcat [constExpr msb, colon, constExpr lsb]

-- | Convert the 'PortDir' type to 'Text'.
portDir :: PortDir -> Doc a
portDir PortIn = "input"
portDir PortOut = "output"
portDir PortInOut = "inout"

-- | Generate a '(ModItem ann)'.
moduleItem :: Annotation ann => ModItem ann -> Doc a
moduleItem (ModCA _ ca) = contAssign ca
moduleItem (ModInst _ i param name conn) =
  (<> semi) $
    hsep
      [ identifier i,
        "#" <> (parens . hsep $ punctuate comma (mConn <$> param)),
        identifier name,
        parens . hsep $ punctuate comma (mConn <$> conn)
      ]
moduleItem (Initial _ stat) = nest 2 $ vsep ["initial", statement stat]
moduleItem (Always _ stat) = nest 2 $ vsep ["always", statement stat]
moduleItem (Decl _ dir p ini) =
  (<> semi) . hsep
    . addMay (portDir <$> dir)
    . (port p :)
    $ addMay (makeIni <$> ini) []
  where
    makeIni = ("=" <+>) . constExpr
moduleItem (ParamDecl _ p) = hcat [paramList p, semi]
moduleItem (LocalParamDecl _ p) = hcat [localParamList p, semi]
-- moduleItem (ModItemAnn a mi) = sep [hsep ["/*", pretty $ show a, "*/"], moduleItem mi]
moduleItem (Property _ l e bl br) =
  sep [hcat [identifier l, ":"], "assume property", parens $ event e,
       hcat [case bl of
               Just bl' -> sep [expr bl', "|=>", expr br]
               Nothing -> expr br, semi]
      ]

mConn :: Annotation ann => ModConn ann -> Doc a
mConn (ModConn c) = expr c
mConn (ModConnNamed n c) = hcat [dot, identifier n, parens $ expr c]

-- | Generate continuous assignment
contAssign :: Annotation ann => ContAssign ann -> Doc a
contAssign (ContAssign val e) =
  (<> semi) $ hsep ["assign", identifier val, "=", align $ expr e]

ann :: Show a => a -> Doc b
ann a = hsep ["/*", pretty (show a), "*/"]

-- | Generate 'Expr' to 'Text'.
expr :: Annotation ann => Expr ann -> Doc a
expr (BinOp eAnn eRhs bin eLhs) = parens $ hsep [ann eAnn, expr eRhs, binaryOp bin, expr eLhs]
expr (Number eAnn b) = parens $ ann eAnn <+> showNum b
expr (Id eAnn i) = parens $ ann eAnn <+> identifier i
expr (VecSelect eAnn i e) = parens $ ann eAnn <+> hcat [identifier i, brackets $ expr e]
expr (RangeSelect eAnn i r) = parens $ ann eAnn <+> hcat [identifier i, range r]
expr (Concat eAnn c) = parens $ ann eAnn <+> (braces . nest 4 . sep . punctuate comma $ toList (expr <$> c))
expr (UnOp eAnn u e) = parens $ ann eAnn <+> hcat [unaryOp u, expr e]
expr (Cond eAnn l t f) =
  parens . nest 4 $ sep [ann eAnn, expr l, "?" <+> expr t, colon <+> expr f]
expr (Appl eAnn f e) = parens $ ann eAnn <+> hcat [identifier f, parens $ expr e]
expr (Str eAnn t) = parens (ann eAnn <+> dquotes (pretty t))

showNum :: BitVec -> Doc a
showNum (BitVec s n) =
    hcat [minus, pretty $ showT s, "'h", pretty $ T.pack (showHex (abs n) "")]
  where
    minus
      | signum n >= 0 = mempty
      | otherwise = "-"

constExpr :: ConstExpr ann -> Doc a
constExpr (ConstNum _ b) = showNum b
constExpr (ParamId _ i) = identifier i
constExpr (ConstConcat _ c) =
  braces . hsep . punctuate comma $ toList (constExpr <$> c)
constExpr (ConstUnOp _ u e) = parens $ hcat [unaryOp u, constExpr e]
constExpr (ConstBinOp _ eRhs bin eLhs) =
  parens $ hsep [constExpr eRhs, binaryOp bin, constExpr eLhs]
constExpr (ConstCond _ l t f) =
  parens $ hsep [constExpr l, "?", constExpr t, colon, constExpr f]
constExpr (ConstStr _ t) = dquotes $ pretty t

-- | Convert 'BinaryOperator' to 'Text'.
binaryOp :: BinaryOperator -> Doc a
binaryOp BinPlus = "+"
binaryOp BinMinus = "-"
binaryOp BinTimes = "*"
binaryOp BinDiv = "/"
binaryOp BinMod = "%"
binaryOp BinEq = "=="
binaryOp BinNEq = "!="
binaryOp BinCEq = "==="
binaryOp BinCNEq = "!=="
binaryOp BinLAnd = "&&"
binaryOp BinLOr = "||"
binaryOp BinLT = "<"
binaryOp BinLEq = "<="
binaryOp BinGT = ">"
binaryOp BinGEq = ">="
binaryOp BinAnd = "&"
binaryOp BinOr = "|"
binaryOp BinXor = "^"
binaryOp BinXNor = "^~"
binaryOp BinXNorInv = "~^"
binaryOp BinPower = "**"
binaryOp BinLSL = "<<"
binaryOp BinLSR = ">>"
binaryOp BinASL = "<<<"
binaryOp BinASR = ">>>"

-- | Convert 'UnaryOperator' to 'Text'.
unaryOp :: UnaryOperator -> Doc a
unaryOp UnPlus = "+"
unaryOp UnMinus = "-"
unaryOp UnLNot = "!"
unaryOp UnNot = "~"
unaryOp UnAnd = "&"
unaryOp UnNand = "~&"
unaryOp UnOr = "|"
unaryOp UnNor = "~|"
unaryOp UnXor = "^"
unaryOp UnNxor = "~^"
unaryOp UnNxorInv = "^~"

event :: Annotation ann => Event ann -> Doc a
event a = hcat ["@", parens $ eventRec a]

-- | Generate verilog code for an 'Event'.
eventRec :: Annotation ann => Event ann -> Doc a
eventRec (EId i) = identifier i
eventRec (EExpr e) = expr e
eventRec EAll = "*"
eventRec (EPosEdge i) = hsep ["posedge", identifier i]
eventRec (ENegEdge i) = hsep ["negedge", identifier i]
eventRec (EOr a b) = hsep [eventRec a, "or", eventRec b]
eventRec (EComb a b) = hsep $ punctuate comma [eventRec a, eventRec b]

-- | Generates verilog code for a 'Delay'.
delay :: Delay -> Doc a
delay (Delay i) = "#" <> pretty i

-- | Generate the verilog code for an 'LVal'.
lVal :: Annotation ann => LVal ann -> Doc a
lVal (RegId i) = identifier i
lVal (RegExpr i e) = hsep [identifier i, expr e]
lVal (RegSize i r) = hsep [identifier i, range r]
lVal (RegConcat e) = braces . hsep $ punctuate comma (expr <$> e)

pType :: PortType -> Doc a
pType Wire = "wire"
pType Reg = "reg"

genAssign :: Annotation ann => Text -> Assign ann -> Doc a
genAssign op (Assign r d e) =
  hsep . (lVal r :) . (pretty op :) $ addMay (delay <$> d) [expr e]

caseType :: CaseType -> Doc a
caseType CaseStandard = "case"
caseType CaseX = "casex"
caseType CaseZ = "casez"

casePair :: Annotation ann => CasePair ann -> Doc a
casePair (CasePair e s) =
  vsep [hsep [expr e, colon], indent 2 $ statement s]

statement :: Annotation ann => Statement ann -> Doc a
statement (TimeCtrl _ d stat) = hsep [delay d, defMap stat]
statement (EventCtrl _ e stat) = hsep [event e, defMap stat]
statement (SeqBlock _ s) =
  vsep ["begin", indent 2 . vsep $ statement <$> s, "end"]
statement (BlockAssign _ a) = hcat [genAssign "=" a, semi]
statement (NonBlockAssign _ a) = hcat [genAssign "<=" a, semi]
statement (TaskEnable _ t) = hcat [task t, semi]
statement (SysTaskEnable _ t) = hcat ["$", task t, semi]
statement (CondStmnt _ e t Nothing) =
  vsep [hsep ["if", parens $ expr e], indent 2 $ defMap t]
statement (StmntCase _ t e ls d) =
  vcat
    [ hcat [caseType t, parens $ expr e],
      vcat $ casePair <$> ls,
      indent 2 $ vsep ["default:", indent 2 $ defMap d],
      "endcase"
    ]
statement (CondStmnt _ e t f) =
  vsep
    [ hsep ["if", parens $ expr e],
      indent 2 $ defMap t,
      "else",
      indent 2 $ defMap f
    ]
statement (ForLoop _ a e incr stmnt) =
  vsep
    [ hsep
        [ "for",
          parens . hsep $
            punctuate
              semi
              [genAssign "=" a, expr e, genAssign "=" incr]
        ],
      indent 2 $ statement stmnt
    ]
-- statement (StmntAnn a s) = sep [hsep ["/*", pretty $ show a, "*/"], statement s]

task :: Annotation ann => Task ann -> Doc a
task (Task i e)
  | null e = identifier i
  | otherwise =
    hsep
      [identifier i, parens . hsep $ punctuate comma (expr <$> e)]

-- | Render the 'Text' to 'IO'. This is equivalent to 'putStrLn'.
render :: (Source a) => a -> IO ()
render = print . genSource

-- Instances

instance Source Identifier where
  genSource = showT . identifier

instance Annotation ann => Source (Task ann) where
  genSource = showT . task

instance Annotation ann => Source (Statement ann) where
  genSource = showT . statement

instance Source PortType where
  genSource = showT . pType

instance Annotation ann => Source (ConstExpr ann) where
  genSource = showT . constExpr

instance Annotation ann => Source (LVal ann) where
  genSource = showT . lVal

instance Source Delay where
  genSource = showT . delay

instance Annotation ann => Source (Event ann) where
  genSource = showT . event

instance Source UnaryOperator where
  genSource = showT . unaryOp

instance Annotation ann => Source (Expr ann) where
  genSource = showT . expr

instance Annotation ann => Source (ContAssign ann) where
  genSource = showT . contAssign

instance Annotation ann => Source (ModItem ann) where
  genSource = showT . moduleItem

instance Source PortDir where
  genSource = showT . portDir

instance Source (Port ann) where
  genSource = showT . port

instance Annotation ann => Source (ModDecl ann) where
  genSource = showT . moduleDecl

instance Annotation ann => Source (Verilog ann) where
  genSource = showT . verilogSrc

instance Annotation ann => Source (SourceInfo ann) where
  genSource (SourceInfo _ src) = genSource src

newtype GenVerilog a = GenVerilog {unGenVerilog :: a}
  deriving (Eq, Ord, Data)

instance (Source a) => Show (GenVerilog a) where
  show = T.unpack . genSource . unGenVerilog
