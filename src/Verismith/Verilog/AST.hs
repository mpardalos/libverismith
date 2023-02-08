{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Verismith.Verilog.AST
-- Description : Definition of the Verilog AST types.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Poratbility : POSIX
--
-- Defines the types to build a Verilog AST.
module Verismith.Verilog.AST
  ( -- * Top level types
    SourceInfo (..),
    Verilog (..),

    -- * Primitives

    -- ** Identifier
    Identifier (..),

    -- ** Control
    Delay (..),
    Event (..),

    -- ** Operators
    BinaryOperator (..),
    UnaryOperator (..),

    -- ** Task
    Task (..),

    -- ** Left hand side value
    LVal (..),

    -- ** Ports
    PortDir (..),
    PortType (..),
    Port (..),

    -- * Expression
    Expr (..),
    ConstExpr (..),
    ConstExprF (..),
    constToExpr,
    exprToConst,
    Range (..),

    -- * Assignment
    Assign (..),
    ContAssign (..),

    -- ** Parameters
    Parameter (..),
    LocalParam (..),

    -- * Statment
    CaseType (..),
    CasePair (..),
    Statement (..),

    -- * Module
    ModDecl (..),
    ModItem (..),
    traverseModItem,
    ModConn (..),

    -- * Useful Lenses and Traversals
    Annotated(..),
    aModule,
    getModule,
    getSourceId,
    mainModule,
  )
where

import Control.DeepSeq (NFData)
import Data.Data
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.String (IsString, fromString)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Optics (Lens', Traversal', lens, traversed, (%), (%~), (&), (^..))
import Optics.TH
import Verismith.Verilog.BitVec

class Annotated a where
  type Ann a
  annotation :: Lens' a (Ann a)

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier where
  Identifier :: {getIdentifier :: Text} -> Identifier
  deriving (Eq, Show, Ord, Data, Generic)
  deriving newtype (NFData)

makePrismLabels ''Identifier

instance IsString Identifier where
  fromString = Identifier . pack

instance Semigroup Identifier where
  Identifier a <> Identifier b = Identifier $ a <> b

instance Monoid Identifier where
  mempty = Identifier mempty

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay {_getDelay :: Int}
  deriving (Eq, Show, Ord, Data, Generic)
  deriving newtype (NFData)

makePrismLabels ''Delay

instance Num Delay where
  Delay a + Delay b = Delay $ a + b
  Delay a - Delay b = Delay $ a - b
  Delay a * Delay b = Delay $ a * b
  negate (Delay a) = Delay $ negate a
  abs (Delay a) = Delay $ abs a
  signum (Delay a) = Delay $ signum a
  fromInteger = Delay . fromInteger

-- | Binary operators that are currently supported in the verilog generation.
data BinaryOperator
  = BinPlus
  | BinMinus
  | BinTimes
  | BinDiv
  | BinMod
  | BinEq
  | BinNEq
  | BinCEq
  | BinCNEq
  | BinLAnd
  | BinLOr
  | BinLT
  | BinLEq
  | BinGT
  | BinGEq
  | BinAnd
  | BinOr
  | BinXor
  | BinXNor
  | BinXNorInv
  | BinPower
  | BinLSL
  | BinLSR
  | BinASL
  | BinASR
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Unary operators that are currently supported by the generator.
data UnaryOperator
  = UnPlus
  | UnMinus
  | UnLNot
  | UnNot
  | UnAnd
  | UnNand
  | UnOr
  | UnNor
  | UnXor
  | UnNxor
  | UnNxorInv
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Constant expression, which are known before simulation at compile time.
data ConstExpr ann
  = ConstNum
      { annotation :: ann,
        num :: {-# UNPACK #-} !BitVec
      }
  | ParamId
      { annotation :: ann,
        identifier :: {-# UNPACK #-} !Identifier
      }
  | ConstConcat
      { annotation :: ann,
        concatExprs :: !(NonEmpty (ConstExpr ann))
      }
  | ConstUnOp
      { annotation :: ann,
        operator :: !UnaryOperator,
        expr :: !(ConstExpr ann)
      }
  | ConstBinOp
      { annotation :: ann,
        lhs :: !(ConstExpr ann),
        binOp :: !BinaryOperator,
        rhs :: !(ConstExpr ann)
      }
  | ConstCond
      { annotation :: ann,
        cond :: !(ConstExpr ann),
        trueExpr :: !(ConstExpr ann),
        falseExpr :: !(ConstExpr ann)
      }
  | ConstStr
      { annotation :: ann,
        str :: {-# UNPACK #-} !Text
      }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''ConstExpr

$(makeBaseFunctor ''ConstExpr)

constToExpr :: ConstExpr ann  -> Expr ann
constToExpr (ConstNum ann a) = Number ann a
constToExpr (ParamId ann a) = Id ann a
constToExpr (ConstConcat ann a) = Concat ann $ fmap constToExpr a
constToExpr (ConstUnOp ann a b) = UnOp ann a $ constToExpr b
constToExpr (ConstBinOp ann a b c) = BinOp ann (constToExpr a) b $ constToExpr c
constToExpr (ConstCond ann a b c) = Cond ann (constToExpr a) (constToExpr b) $ constToExpr c
constToExpr (ConstStr ann a) = Str ann a

exprToConst :: Expr ann -> ConstExpr ann
exprToConst (Number ann a) = ConstNum ann a
exprToConst (Id ann a) = ParamId ann a
exprToConst (Concat ann a) = ConstConcat ann $ fmap exprToConst a
exprToConst (UnOp ann a b) = ConstUnOp ann a $ exprToConst b
exprToConst (BinOp ann a b c) = ConstBinOp ann (exprToConst a) b $ exprToConst c
exprToConst (Cond ann a b c) = ConstCond ann (exprToConst a) (exprToConst b) $ exprToConst c
exprToConst (Str ann a) = ConstStr ann a
exprToConst _ = error "Not a constant expression"

instance Monoid ann => Num (ConstExpr ann) where
  a + b = ConstBinOp mempty a BinPlus b
  a - b = ConstBinOp mempty a BinMinus b
  a * b = ConstBinOp mempty a BinTimes b
  negate = ConstUnOp mempty UnMinus
  abs = undefined
  signum = undefined
  fromInteger = ConstNum mempty . fromInteger

instance Monoid ann => Semigroup (ConstExpr ann) where
  (ConstConcat ann1 a) <> (ConstConcat ann2 b) = ConstConcat (ann1 <> ann2) (a <> b)
  (ConstConcat ann a) <> b = ConstConcat ann $ a <> (b :| [])
  a <> (ConstConcat ann b) = ConstConcat ann $ a <| b
  a <> b = ConstConcat mempty $ a <| b :| []

instance Monoid ann => Monoid (ConstExpr ann) where
  mempty = ConstNum mempty 0

instance Monoid ann => IsString (ConstExpr ann) where
  fromString = ConstStr mempty . fromString

-- | Range that can be associated with any port or left hand side. Contains the
-- msb and lsb bits as 'ConstExpr'. This means that they can be generated using
-- parameters, which can in turn be changed at synthesis time.
data Range ann = Range
  { rangeMSB :: !(ConstExpr ann),
    rangeLSB :: !(ConstExpr ann)
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

instance Monoid ann => Num (Range ann) where
  (Range s1 a) + (Range s2 b) = Range (s1 + s2) (a + b)
  (Range s1 a) - (Range s2 b) = Range (s1 - s2) (a - b)
  (Range s1 a) * (Range s2 b) = Range (s1 * s2) (a * b)
  negate = undefined
  abs = id
  signum _ = 1
  fromInteger = flip Range 0 . fromInteger . (-) 1

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr ann
  = Number ann {-# UNPACK #-} !BitVec
  | Id ann {-# UNPACK #-} !Identifier
  | VecSelect ann {-# UNPACK #-} !Identifier !(Expr ann)
  | RangeSelect ann {-# UNPACK #-} !Identifier !(Range ann)
  | Concat ann !(NonEmpty (Expr ann))
  | UnOp ann !UnaryOperator !(Expr ann)
  | BinOp ann !(Expr ann) !BinaryOperator !(Expr ann)
  | Cond ann !(Expr ann) !(Expr ann) !(Expr ann)
  | Appl ann !Identifier !(Expr ann)
  | Str ann {-# UNPACK #-} !Text
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''Expr
makePrismLabels ''Expr

$(makeBaseFunctor ''Expr)

instance Annotated (Expr ann) where
  type Ann (Expr ann) = ann

instance Monoid ann => Num (Expr ann) where
  a + b = BinOp mempty a BinPlus b
  a - b = BinOp mempty a BinMinus b
  a * b = BinOp mempty a BinTimes b
  negate = UnOp mempty UnMinus
  abs = undefined
  signum = undefined
  fromInteger = Number mempty . fromInteger

instance Monoid ann => Semigroup (Expr ann) where
  (Concat ann1 a) <> (Concat ann2 b) = Concat (ann1 <> ann2) (a <> b)
  (Concat ann a) <> b = Concat ann $ a <> (b :| [])
  a <> (Concat ann b) = Concat ann $ a <| b
  a <> b = Concat mempty $ a <| b :| []

instance Monoid ann => Monoid (Expr ann) where
  mempty = Number mempty 0

instance Monoid ann => IsString (Expr ann) where
  fromString = Str mempty . fromString

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event ann
  = EId {-# UNPACK #-} !Identifier
  | EExpr !(Expr ann)
  | EAll
  | EPosEdge {-# UNPACK #-} !Identifier
  | ENegEdge {-# UNPACK #-} !Identifier
  | EOr !(Event ann) !(Event ann)
  | EComb !(Event ann) !(Event ann)
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

$(makeBaseFunctor ''Event)

-- | Task call, which is similar to function calls.
data Task ann = Task
  { name :: {-# UNPACK #-} !Identifier,
    args :: [Expr ann]
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''Task

-- | Type that represents the left hand side of an assignment, which can be a
-- concatenation such as in:
--
-- @
-- {a, b, c} = 32'h94238;
-- @
data LVal ann
  = RegId {-# UNPACK #-} !Identifier
  | RegExpr
      { ident :: {-# UNPACK #-} !Identifier,
        expr :: !(Expr ann)
      }
  | RegSize
      { ident :: {-# UNPACK #-} !Identifier,
        range :: {-# UNPACK #-} !(Range ann)
      }
  | RegConcat [Expr ann]
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''LVal

instance IsString (LVal ann) where
  fromString = RegId . fromString

-- | Different port direction that are supported in Verilog.
data PortDir
  = PortIn
  | PortOut
  | PortInOut
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType
  = Wire
  | Reg
  deriving (Eq, Show, Ord, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''PortType

-- | Port declaration. It contains information about the type of the port, the
-- size, and the port name. It used to also contain information about if it was
-- an input or output port. However, this is not always necessary and was more
-- cumbersome than useful, as a lot of ports can be declared without input and
-- output port.
--
-- This is now implemented inside '(ModDecl ann)' itself, which uses a list of output
-- and input ports.
data Port ann = Port
  { portType :: !PortType,
    signed :: !Bool,
    size :: {-# UNPACK #-} !(Range ann),
    name :: {-# UNPACK #-} !Identifier
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''Port

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
data ModConn ann
  = ModConn !(Expr ann)
  | ModConnNamed
      { name :: {-# UNPACK #-} !Identifier,
        expr :: !(Expr ann)
      }
  deriving (Eq, Show, Ord, Data, Functor, Generic, NFData)

makeFieldLabelsNoPrefix ''ModConn

data Assign ann = Assign
  { lval :: !(LVal ann),
    delay :: !(Maybe Delay),
    expr :: !(Expr ann)
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''Assign

-- | Type for continuous assignment.
--
-- @
-- assign x = 2'b1;
-- @
data ContAssign ann = ContAssign
  { lval :: {-# UNPACK #-} !Identifier,
    expr :: !(Expr ann)
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''ContAssign

-- | Case pair which contains an expression followed by a statement which will
-- get executed if the expression matches the expression in the case statement.
data CasePair ann = CasePair
  { expr :: !(Expr ann),
    stmnt :: !(Statement ann)
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

-- | Type of case statement, which determines how it is interpreted.
data CaseType
  = CaseStandard
  | CaseX
  | CaseZ
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Statements in Verilog.
data Statement ann
  = -- | Time control (@#NUM@)
    TimeCtrl
      { annotation :: ann,
        delay :: {-# UNPACK #-} !Delay,
        timeCtrlStatement :: Maybe (Statement ann)
      }
  | EventCtrl
      { annotation :: ann,
        event :: !(Event ann),
        eventCtrlStatement :: Maybe (Statement ann)
      }
  | -- | Sequential block (@begin ... end@)
    SeqBlock ann [Statement ann]
  | -- | blocking assignment (@=@)
    BlockAssign ann !(Assign ann)
  | -- | Non blocking assignment (@<=@)
    NonBlockAssign ann !(Assign ann)
  | TaskEnable ann !(Task ann)
  | SysTaskEnable ann !(Task ann)
  | CondStmnt
      { annotation :: ann,
        cond :: Expr ann,
        trueStatement :: Maybe (Statement ann),
        falseStatement :: Maybe (Statement ann)
      }
  | StmntCase
      { annotation :: ann,
        caseType :: !CaseType,
        expr :: !(Expr ann),
        casePairs :: ![CasePair ann],
        defaultCase :: !(Maybe (Statement ann))
      }
  | -- | Loop bounds shall be statically computable for a for loop.
    ForLoop
      { annotation :: ann,
        forAssign :: !(Assign ann),
        forExpr :: Expr ann,
        forIncr :: !(Assign ann),
        forStmnt :: Statement ann
      }
  deriving (Eq, Show, Ord, Data, Functor, Generic, NFData)

makeFieldLabelsNoPrefix ''Statement

instance Monoid ann => Semigroup (Statement ann) where
  (SeqBlock ann1 a) <> (SeqBlock ann2 b) = SeqBlock (ann1 <> ann2) (a <> b)
  (SeqBlock ann a) <> b = SeqBlock ann $ a <> [b]
  a <> (SeqBlock ann b) = SeqBlock ann $ a : b
  a <> b = SeqBlock mempty [a, b]

instance Monoid a => Monoid (Statement a) where
  mempty = SeqBlock mempty []

-- | Parameter that can be assigned in blocks or modules using @parameter@.
data Parameter ann = Parameter
  { ident :: {-# UNPACK #-} !Identifier,
    value :: ConstExpr ann
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''Parameter

-- | Local parameter that can be assigned anywhere using @localparam@. It cannot
-- be changed by initialising the module.
data LocalParam ann = LocalParam
  { ident :: {-# UNPACK #-} !Identifier,
    value :: ConstExpr ann
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''LocalParam

-- | Module item which is the body of the module expression.
data ModItem ann
  = ModCA ann !(ContAssign ann)
  | ModInst
      { annotation :: ann,
        instId :: {-# UNPACK #-} !Identifier,
        instDecl :: [ModConn ann],
        instName :: {-# UNPACK #-} !Identifier,
        instConns :: [ModConn ann]
      }
  | Initial ann !(Statement ann)
  | Always ann !(Statement ann)
  | Property
      { annotation :: ann,
        propLabel :: {-# UNPACK #-} !Identifier,
        propEvent :: !(Event ann),
        propBodyL :: Maybe (Expr ann),
        propBodyR :: Expr ann
      }
  | Decl
      { annotation :: ann,
        declDir :: !(Maybe PortDir),
        declPort :: !(Port ann),
        declVal :: Maybe (ConstExpr ann)
      }
  | ParamDecl ann (NonEmpty (Parameter ann))
  | LocalParamDecl ann (NonEmpty (LocalParam ann))
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makePrismLabels ''ModItem

makeFieldLabelsNoPrefix ''ModItem

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl ann
  = ModDecl
      { annotation :: ann,
        id :: {-# UNPACK #-} !Identifier,
        outPorts :: ![Port ann],
        inPorts :: ![Port ann],
        items :: ![ModItem ann],
        params :: ![Parameter ann]
      }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''ModDecl
makePrismLabels ''ModDecl

traverseModConn :: (Applicative f) => (Expr ann -> f (Expr ann)) -> ModConn ann -> f (ModConn ann)
traverseModConn f (ModConn e) = ModConn <$> f e
traverseModConn f (ModConnNamed a e) = ModConnNamed a <$> f e

traverseModItem :: (Applicative f) => (Expr ann -> f (Expr ann)) -> ModItem ann -> f (ModItem ann)
traverseModItem f (ModCA ann (ContAssign a e)) = ModCA ann . ContAssign a <$> f e
traverseModItem f (ModInst ann a b c e) =
  ModInst ann a b c <$> traverse (traverseModConn f) e
traverseModItem _ e = pure e

-- | The complete sourcetext for the Verilog module.
newtype Verilog a = Verilog {getVerilog :: [ModDecl a]}
  deriving (Eq, Show, Ord, Functor, Data, Generic)
  deriving newtype (NFData, Semigroup, Monoid)

makePrismLabels ''Verilog

-- | Top level type which contains all the source code and associated
-- information.
data SourceInfo a = SourceInfo
  { top :: {-# UNPACK #-} !Text,
    src :: !(Verilog a)
  }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

makeFieldLabelsNoPrefix ''SourceInfo

instance Semigroup (SourceInfo a) where
  (SourceInfo t v) <> (SourceInfo _ v2) = SourceInfo t $ v <> v2

instance Monoid (SourceInfo a) where
  mempty = SourceInfo mempty mempty

-- | Attributes which can be set to various nodes in the AST.
--
-- @
-- (* synthesis *)
-- @
data Attribute ann
  = AttrAssign Identifier (ConstExpr ann)
  | AttrName Identifier
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Annotations which can be added to the AST. These are supported in all the
-- nodes of the AST and a custom type can be declared for them.
data Annotation ann
  = Ann ann
  | AnnAttrs [Attribute ann]
  deriving (Eq, Show, Ord, Data, Generic, NFData)

getModule :: Traversal' (Verilog a) (ModDecl a)
getModule = #_Verilog % traversed
{-# INLINE getModule #-}

getSourceId :: Traversal' (Verilog a) Text
getSourceId = getModule % #id % #_Identifier
{-# INLINE getSourceId #-}

-- | May need to change this to Traversal to be safe. For now it will fail when
-- the main has not been properly set with.
aModule :: Identifier -> Lens' (SourceInfo a) (ModDecl a)
aModule t = lens get_ set_
  where
    set_ (SourceInfo top main) v =
      SourceInfo top (main & getModule %~ update t.getIdentifier v)
    update top v m@(ModDecl _ (Identifier i) _ _ _ _)
      | i == top = v
      | otherwise = m
    get_ (SourceInfo _ main) =
      head . filter (f t.getIdentifier) $ main ^.. getModule
    f top (ModDecl _ (Identifier i) _ _ _ _) = i == top

-- | May need to change this to Traversal to be safe. For now it will fail when
-- the main has not been properly set with.
mainModule :: Lens' (SourceInfo a) (ModDecl a)
mainModule = lens get_ set_
  where
    set_ (SourceInfo top main) v =
      SourceInfo top (main & getModule %~ update top v)
    update top v m@(ModDecl _ (Identifier i) _ _ _ _)
      | i == top = v
      | otherwise = m
    get_ (SourceInfo top main) = head . filter (f top) $ main ^.. getModule
    f top (ModDecl _ (Identifier i) _ _ _ _) = i == top
