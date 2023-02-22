{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
  ( -- * Annotations
    Annotation (..),
    AnnConstraint,
    Default (def),
    Annotated (..),

    -- * Top level types
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
    ExprF (..),
    ConstExpr (..),
    ConstExprF (..),
    constToExpr,
    exprToConst,
    Range (..),
    rangeFromSize,
    simpleRangeToSize,

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
    aModule,
    getModule,
    getSourceId,
    topModuleId,
    mainModule,
  )
where

import Control.DeepSeq (NFData)
import Data.Data
import Data.Default.Class
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.String (IsString, fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Optics (Lens', Traversal', lens, traversed, (%), (%~), (&), (.~), (^..))
import Optics.TH
import Verismith.Verilog.BitVec

class
  ( AnnConstraint Default ann,
    AnnConstraint Data ann,
    AnnConstraint Show ann,
    Typeable ann,
    Typeable k
  ) =>
  Annotation (ann :: k)
  where
  type AnnExpr ann
  type AnnConstExpr ann
  type AnnStatement ann
  type AnnModItem ann
  type AnnModDecl ann

type AnnConstraint (c :: Type -> Constraint) ann =
  ( c (AnnExpr ann),
    c (AnnConstExpr ann),
    c (AnnStatement ann),
    c (AnnModItem ann),
    c (AnnModDecl ann)
  )

instance Annotation () where
  type AnnExpr () = ()
  type AnnConstExpr () = ()
  type AnnStatement () = ()
  type AnnModItem () = ()
  type AnnModDecl () = ()

class Annotated a where
  setDefaultAnnotations :: forall ann2 ann1. Annotation ann2 => a ann1 -> a ann2

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier {getIdentifier :: Text}
  deriving (Eq, Show, Ord, Data, Generic)
  deriving newtype (NFData, IsString, Semigroup, Monoid)

makePrismLabels ''Identifier

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay {_getDelay :: Int}
  deriving (Eq, Show, Ord, Data, Generic)
  deriving newtype (NFData, Num)

makePrismLabels ''Delay

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
  = ConstNum (AnnConstExpr ann) {-# UNPACK #-} !BitVec
  | ParamId (AnnConstExpr ann) {-# UNPACK #-} !Identifier
  | ConstConcat (AnnConstExpr ann) !(NonEmpty (ConstExpr ann))
  | ConstUnOp (AnnConstExpr ann) !UnaryOperator !(ConstExpr ann)
  | ConstBinOp (AnnConstExpr ann) !(ConstExpr ann) !BinaryOperator !(ConstExpr ann)
  | ConstCond (AnnConstExpr ann) !(ConstExpr ann) !(ConstExpr ann) !(ConstExpr ann)
  | ConstStr (AnnConstExpr ann) {-# UNPACK #-} !Text
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (ConstExpr ann)

deriving instance AnnConstraint Show ann => Show (ConstExpr ann)

deriving instance AnnConstraint Ord ann => Ord (ConstExpr ann)

deriving instance Annotation ann => Data (ConstExpr ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (ConstExpr ann)

instance Annotated ConstExpr where
  setDefaultAnnotations (ConstNum _ a) = ConstNum def a
  setDefaultAnnotations (ParamId _ a) = ParamId def a
  setDefaultAnnotations (ConstConcat _ a) = ConstConcat def (fmap setDefaultAnnotations a)
  setDefaultAnnotations (ConstUnOp _ a b) = ConstUnOp def a (setDefaultAnnotations b)
  setDefaultAnnotations (ConstBinOp _ a b c) = ConstBinOp def (setDefaultAnnotations a) b (setDefaultAnnotations c)
  setDefaultAnnotations (ConstCond _ a b c) = ConstCond def (setDefaultAnnotations a) (setDefaultAnnotations b) (setDefaultAnnotations c)
  setDefaultAnnotations (ConstStr _ a) = ConstStr def a

instance
  (Annotation ann, AnnConstExpr ann ~ annType) =>
  HasField "annotation" (ConstExpr ann) annType
  where
  getField (ConstNum ann _) = ann
  getField (ParamId ann _) = ann
  getField (ConstConcat ann _) = ann
  getField (ConstUnOp ann _ _) = ann
  getField (ConstBinOp ann _ _ _) = ann
  getField (ConstCond ann _ _ _) = ann
  getField (ConstStr ann _) = ann

makeFieldLabelsNoPrefix ''ConstExpr
makeBaseFunctor ''ConstExpr

instance
  (Annotation ann, AnnConstExpr ann ~ annType) =>
  HasField "annotation" (ConstExprF ann a) annType
  where
  getField (ConstNumF ann _) = ann
  getField (ParamIdF ann _) = ann
  getField (ConstConcatF ann _) = ann
  getField (ConstUnOpF ann _ _) = ann
  getField (ConstBinOpF ann _ _ _) = ann
  getField (ConstCondF ann _ _ _) = ann
  getField (ConstStrF ann _) = ann

constToExpr :: (AnnConstExpr ann1 ~ AnnExpr ann2) => ConstExpr ann1 -> Expr ann2
constToExpr (ConstNum ann a) = Number ann a
constToExpr (ParamId ann a) = Id ann a
constToExpr (ConstConcat ann a) = Concat ann $ fmap constToExpr a
constToExpr (ConstUnOp ann a b) = UnOp ann a $ constToExpr b
constToExpr (ConstBinOp ann a b c) = BinOp ann (constToExpr a) b $ constToExpr c
constToExpr (ConstCond ann a b c) = Cond ann (constToExpr a) (constToExpr b) $ constToExpr c
constToExpr (ConstStr ann a) = Str ann a

exprToConst :: (AnnExpr ann1 ~ AnnConstExpr ann2) => Expr ann1 -> ConstExpr ann2
exprToConst (Number ann a) = ConstNum ann a
exprToConst (Id ann a) = ParamId ann a
exprToConst (Concat ann a) = ConstConcat ann $ fmap exprToConst a
exprToConst (UnOp ann a b) = ConstUnOp ann a $ exprToConst b
exprToConst (BinOp ann a b c) = ConstBinOp ann (exprToConst a) b $ exprToConst c
exprToConst (Cond ann a b c) = ConstCond ann (exprToConst a) (exprToConst b) $ exprToConst c
exprToConst (Str ann a) = ConstStr ann a
exprToConst _ = error "Not a constant expression"

instance (Annotation ann, Semigroup (AnnConstExpr ann)) => Semigroup (ConstExpr ann) where
  (ConstConcat ann1 a) <> (ConstConcat ann2 b) = ConstConcat (ann1 <> ann2) (a <> b)
  (ConstConcat ann a) <> b = ConstConcat ann $ a <> (b :| [])
  a <> (ConstConcat ann b) = ConstConcat ann $ a <| b
  a <> b = ConstConcat def $ a <| b :| []

instance (Annotation ann, Semigroup (AnnConstExpr ann)) => Monoid (ConstExpr ann) where
  mempty = ConstNum def 0

instance Annotation ann => IsString (ConstExpr ann) where
  fromString = ConstStr def . fromString

-- | Range that can be associated with any port or left hand side. Contains the
-- msb and lsb bits as 'ConstExpr'. This means that they can be generated using
-- parameters, which can in turn be changed at synthesis time.
data Range ann = Range
  { rangeMSB :: !(ConstExpr ann),
    rangeLSB :: !(ConstExpr ann)
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Range ann)

deriving instance AnnConstraint Show ann => Show (Range ann)

deriving instance AnnConstraint Ord ann => Ord (Range ann)

deriving instance Annotation ann => Data (Range ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Range ann)

instance Annotated Range where
  setDefaultAnnotations (Range a b) = Range (setDefaultAnnotations a) (setDefaultAnnotations b)

-- | Construct a range with the given number of bits
rangeFromSize :: Annotation ann => Integer -> Range ann
rangeFromSize n = Range (ConstNum def (fromInteger n - 1)) (ConstNum def 0)

-- | Get the number of bits in a range, if both ends are given as simple numbers
simpleRangeToSize :: Range ann -> Maybe Integer
simpleRangeToSize (Range (ConstNum _ msb) (ConstNum _ lsb)) = Just (msb.value - lsb.value + 1)
simpleRangeToSize _ = Nothing

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr ann
  = Number (AnnExpr ann) {-# UNPACK #-} !BitVec
  | Id (AnnExpr ann) {-# UNPACK #-} !Identifier
  | VecSelect (AnnExpr ann) {-# UNPACK #-} !Identifier !(Expr ann)
  | RangeSelect (AnnExpr ann) {-# UNPACK #-} !Identifier !(Range ann)
  | Concat (AnnExpr ann) !(NonEmpty (Expr ann))
  | UnOp (AnnExpr ann) !UnaryOperator !(Expr ann)
  | BinOp (AnnExpr ann) !(Expr ann) !BinaryOperator !(Expr ann)
  | Cond (AnnExpr ann) !(Expr ann) !(Expr ann) !(Expr ann)
  | Appl (AnnExpr ann) !Identifier !(Expr ann)
  | Str (AnnExpr ann) {-# UNPACK #-} !Text
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Expr ann)

deriving instance AnnConstraint Show ann => Show (Expr ann)

deriving instance AnnConstraint Ord ann => Ord (Expr ann)

deriving instance Annotation ann => Data (Expr ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Expr ann)

instance Annotated Expr where
  setDefaultAnnotations (Cond _ cond tBranch fBranch) = Cond def (setDefaultAnnotations cond) (setDefaultAnnotations tBranch) (setDefaultAnnotations fBranch)
  setDefaultAnnotations (Number _ a) = Number def a
  setDefaultAnnotations (Id _ a) = Id def a
  setDefaultAnnotations (VecSelect _ a b) = VecSelect def a (setDefaultAnnotations b)
  setDefaultAnnotations (RangeSelect _ a b) = RangeSelect def a (setDefaultAnnotations b)
  setDefaultAnnotations (Concat _ a) = Concat def (setDefaultAnnotations <$> a)
  setDefaultAnnotations (UnOp _ a b) = UnOp def a (setDefaultAnnotations b)
  setDefaultAnnotations (BinOp _ a b c) = BinOp def (setDefaultAnnotations a) b (setDefaultAnnotations c)
  setDefaultAnnotations (Appl _ a b) = Appl def a (setDefaultAnnotations b)
  setDefaultAnnotations (Str _ a) = Str def a

instance
  (Annotation ann, AnnExpr ann ~ annType) =>
  HasField "annotation" (Expr ann) annType
  where
  getField (Number ann _) = ann
  getField (Id ann _) = ann
  getField (VecSelect ann _ _) = ann
  getField (RangeSelect ann _ _) = ann
  getField (Concat ann _) = ann
  getField (UnOp ann _ _) = ann
  getField (BinOp ann _ _ _) = ann
  getField (Cond ann _ _ _) = ann
  getField (Appl ann _ _) = ann
  getField (Str ann _) = ann

makeFieldLabelsNoPrefix ''Expr
makePrismLabels ''Expr
makeBaseFunctor ''Expr

instance
  (Annotation ann, AnnExpr ann ~ annType) =>
  HasField "annotation" (ExprF ann a) annType
  where
  getField (NumberF ann _) = ann
  getField (IdF ann _) = ann
  getField (VecSelectF ann _ _) = ann
  getField (RangeSelectF ann _ _) = ann
  getField (ConcatF ann _) = ann
  getField (UnOpF ann _ _) = ann
  getField (BinOpF ann _ _ _) = ann
  getField (CondF ann _ _ _) = ann
  getField (ApplF ann _ _) = ann
  getField (StrF ann _) = ann

instance (Semigroup (AnnExpr ann), Annotation ann) => Semigroup (Expr ann) where
  (Concat ann1 a) <> (Concat ann2 b) = Concat (ann1 <> ann2) (a <> b)
  (Concat ann a) <> b = Concat ann $ a <> (b :| [])
  a <> (Concat ann b) = Concat ann $ a <| b
  a <> b = Concat def $ a <| b :| []

instance (Semigroup (AnnExpr ann), Annotation ann) => Monoid (Expr ann) where
  mempty = Number def 0

instance Annotation ann => IsString (Expr ann) where
  fromString = Str def . fromString

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event ann
  = EId {-# UNPACK #-} !Identifier
  | EExpr !(Expr ann)
  | EAll
  | EPosEdge {-# UNPACK #-} !Identifier
  | ENegEdge {-# UNPACK #-} !Identifier
  | EOr !(Event ann) !(Event ann)
  | EComb !(Event ann) !(Event ann)
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Event ann)

deriving instance AnnConstraint Show ann => Show (Event ann)

deriving instance AnnConstraint Ord ann => Ord (Event ann)

deriving instance Annotation ann => Data (Event ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Event ann)

instance Annotated Event where
  setDefaultAnnotations (EId a) = EId a
  setDefaultAnnotations (EExpr a) = EExpr (setDefaultAnnotations a)
  setDefaultAnnotations EAll = EAll
  setDefaultAnnotations (EPosEdge a) = EPosEdge a
  setDefaultAnnotations (ENegEdge a) = ENegEdge a
  setDefaultAnnotations (EOr a b) = EOr (setDefaultAnnotations a) (setDefaultAnnotations b)
  setDefaultAnnotations (EComb a b) = EComb (setDefaultAnnotations a) (setDefaultAnnotations b)

makeBaseFunctor ''Event

-- | Task call, which is similar to function calls.
data Task ann = Task
  { name :: {-# UNPACK #-} !Identifier,
    args :: [Expr ann]
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Task ann)

deriving instance AnnConstraint Show ann => Show (Task ann)

deriving instance AnnConstraint Ord ann => Ord (Task ann)

deriving instance Annotation ann => Data (Task ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Task ann)

instance Annotated Task where
  setDefaultAnnotations (Task a b) = Task a (setDefaultAnnotations <$> b)

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
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (LVal ann)

deriving instance AnnConstraint Show ann => Show (LVal ann)

deriving instance AnnConstraint Ord ann => Ord (LVal ann)

deriving instance Annotation ann => Data (LVal ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (LVal ann)

makeFieldLabelsNoPrefix ''LVal

instance Annotated LVal where
  setDefaultAnnotations (RegId a) = RegId a
  setDefaultAnnotations (RegExpr a b) = RegExpr a (setDefaultAnnotations b)
  setDefaultAnnotations (RegSize a b) = RegSize a (setDefaultAnnotations b)
  setDefaultAnnotations (RegConcat a) = RegConcat (setDefaultAnnotations <$> a)

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
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Port ann)

deriving instance AnnConstraint Show ann => Show (Port ann)

deriving instance AnnConstraint Ord ann => Ord (Port ann)

deriving instance Annotation ann => Data (Port ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Port ann)

instance Annotated Port where
  setDefaultAnnotations (Port a b c d) = Port a b (setDefaultAnnotations c) d

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
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (ModConn ann)

deriving instance AnnConstraint Show ann => Show (ModConn ann)

deriving instance AnnConstraint Ord ann => Ord (ModConn ann)

deriving instance Annotation ann => Data (ModConn ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (ModConn ann)

instance Annotated ModConn where
  setDefaultAnnotations (ModConn a) = ModConn (setDefaultAnnotations a)
  setDefaultAnnotations (ModConnNamed a b) = ModConnNamed a (setDefaultAnnotations b)

makeFieldLabelsNoPrefix ''ModConn

data Assign ann = Assign
  { lval :: !(LVal ann),
    delay :: !(Maybe Delay),
    expr :: !(Expr ann)
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Assign ann)

deriving instance AnnConstraint Show ann => Show (Assign ann)

deriving instance AnnConstraint Ord ann => Ord (Assign ann)

deriving instance Annotation ann => Data (Assign ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Assign ann)

instance Annotated Assign where
  setDefaultAnnotations (Assign a b c) = Assign (setDefaultAnnotations a) b (setDefaultAnnotations c)

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
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (ContAssign ann)

deriving instance AnnConstraint Show ann => Show (ContAssign ann)

deriving instance AnnConstraint Ord ann => Ord (ContAssign ann)

deriving instance Annotation ann => Data (ContAssign ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (ContAssign ann)

instance Annotated ContAssign where
  setDefaultAnnotations (ContAssign a b) = ContAssign a (setDefaultAnnotations b)

makeFieldLabelsNoPrefix ''ContAssign

-- | Case pair which contains an expression followed by a statement which will
-- get executed if the expression matches the expression in the case statement.
data CasePair ann = CasePair
  { expr :: !(Expr ann),
    stmnt :: !(Statement ann)
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (CasePair ann)

deriving instance AnnConstraint Show ann => Show (CasePair ann)

deriving instance AnnConstraint Ord ann => Ord (CasePair ann)

deriving instance Annotation ann => Data (CasePair ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (CasePair ann)

instance Annotated CasePair where
  setDefaultAnnotations (CasePair a b) = CasePair (setDefaultAnnotations a) (setDefaultAnnotations b)

-- | Type of case statement, which determines how it is interpreted.
data CaseType
  = CaseStandard
  | CaseX
  | CaseZ
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Statements in Verilog.
data Statement ann
  = -- | Time control (@#NUM@)
    TimeCtrl (AnnStatement ann) {-# UNPACK #-} !Delay (Maybe (Statement ann))
  | EventCtrl
      { eventAnnotation :: AnnStatement ann,
        event :: !(Event ann),
        eventCtrlStatement :: Maybe (Statement ann)
      }
  | -- | Sequential block (@begin ... end@)
    SeqBlock (AnnStatement ann) [Statement ann]
  | -- | blocking assignment (@=@)
    BlockAssign (AnnStatement ann) !(Assign ann)
  | -- | Non blocking assignment (@<=@)
    NonBlockAssign (AnnStatement ann) !(Assign ann)
  | TaskEnable (AnnStatement ann) !(Task ann)
  | SysTaskEnable (AnnStatement ann) !(Task ann)
  | CondStmnt
      { condAnnotation :: AnnStatement ann,
        cond :: Expr ann,
        trueStatement :: Maybe (Statement ann),
        falseStatement :: Maybe (Statement ann)
      }
  | StmntCase
      { caseAnnotation :: AnnStatement ann,
        caseType :: !CaseType,
        expr :: !(Expr ann),
        casePairs :: ![CasePair ann],
        defaultCase :: !(Maybe (Statement ann))
      }
  | -- | Loop bounds shall be statically computable for a for loop.
    ForLoop
      { forAnnotation :: AnnStatement ann,
        forAssign :: !(Assign ann),
        forExpr :: Expr ann,
        forIncr :: !(Assign ann),
        forStmnt :: Statement ann
      }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Statement ann)

deriving instance AnnConstraint Show ann => Show (Statement ann)

deriving instance AnnConstraint Ord ann => Ord (Statement ann)

deriving instance Annotation ann => Data (Statement ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Statement ann)

instance Annotated Statement where
  setDefaultAnnotations (TimeCtrl _ a b) = TimeCtrl def a (setDefaultAnnotations <$> b)
  setDefaultAnnotations (EventCtrl _ a b) = EventCtrl def (setDefaultAnnotations a) (setDefaultAnnotations <$> b)
  setDefaultAnnotations (SeqBlock _ a) = SeqBlock def (setDefaultAnnotations <$> a)
  setDefaultAnnotations (BlockAssign _ a) = BlockAssign def (setDefaultAnnotations a)
  setDefaultAnnotations (NonBlockAssign _ a) = NonBlockAssign def (setDefaultAnnotations a)
  setDefaultAnnotations (TaskEnable _ a) = TaskEnable def (setDefaultAnnotations a)
  setDefaultAnnotations (SysTaskEnable _ a) = SysTaskEnable def (setDefaultAnnotations a)
  setDefaultAnnotations (CondStmnt _ a b c) = CondStmnt def (setDefaultAnnotations a) (setDefaultAnnotations <$> b) (setDefaultAnnotations <$> c)
  setDefaultAnnotations (StmntCase _ a b c d) = StmntCase def a (setDefaultAnnotations b) (setDefaultAnnotations <$> c) (setDefaultAnnotations <$> d)
  setDefaultAnnotations (ForLoop _ a b c d) = ForLoop def (setDefaultAnnotations a) (setDefaultAnnotations b) (setDefaultAnnotations c) (setDefaultAnnotations d)

instance
  AnnStatement ann ~ annType =>
  HasField "annotation" (Statement ann) annType
  where
  getField (TimeCtrl ann _ _) = ann
  getField (EventCtrl ann _ _) = ann
  getField (SeqBlock ann _) = ann
  getField (BlockAssign ann _) = ann
  getField (NonBlockAssign ann _) = ann
  getField (TaskEnable ann _) = ann
  getField (SysTaskEnable ann _) = ann
  getField (CondStmnt ann _ _ _) = ann
  getField (StmntCase ann _ _ _ _) = ann
  getField (ForLoop ann _ _ _ _) = ann

makeFieldLabelsNoPrefix ''Statement
makePrisms ''Statement

instance (Annotation ann, Semigroup (AnnStatement ann)) => Semigroup (Statement ann) where
  (SeqBlock ann1 a) <> (SeqBlock ann2 b) = SeqBlock (ann1 <> ann2) (a <> b)
  (SeqBlock ann a) <> b = SeqBlock ann $ a <> [b]
  a <> (SeqBlock ann b) = SeqBlock ann $ a : b
  a <> b = SeqBlock def [a, b]

instance (Annotation a, Semigroup (AnnStatement a)) => Monoid (Statement a) where
  mempty = SeqBlock def []

-- | Parameter that can be assigned in blocks or modules using @parameter@.
data Parameter ann = Parameter
  { ident :: {-# UNPACK #-} !Identifier,
    value :: ConstExpr ann
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (Parameter ann)

deriving instance AnnConstraint Show ann => Show (Parameter ann)

deriving instance AnnConstraint Ord ann => Ord (Parameter ann)

deriving instance Annotation ann => Data (Parameter ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Parameter ann)

instance Annotated Parameter where
  setDefaultAnnotations (Parameter a b) = Parameter a (setDefaultAnnotations b)

makeFieldLabelsNoPrefix ''Parameter

-- | Local parameter that can be assigned anywhere using @localparam@. It cannot
-- be changed by initialising the module.
data LocalParam ann = LocalParam
  { ident :: {-# UNPACK #-} !Identifier,
    value :: ConstExpr ann
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (LocalParam ann)

deriving instance AnnConstraint Show ann => Show (LocalParam ann)

deriving instance AnnConstraint Ord ann => Ord (LocalParam ann)

deriving instance Annotation ann => Data (LocalParam ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (LocalParam ann)

instance Annotated LocalParam where
  setDefaultAnnotations (LocalParam a b) = LocalParam a (setDefaultAnnotations b)

makeFieldLabelsNoPrefix ''LocalParam

-- | Module item which is the body of the module expression.
data ModItem ann
  = ModCA (AnnModItem ann) !(ContAssign ann)
  | ModInst
      { instAnnotation :: AnnModItem ann,
        instId :: {-# UNPACK #-} !Identifier,
        instDecl :: [ModConn ann],
        instName :: {-# UNPACK #-} !Identifier,
        instConns :: [ModConn ann]
      }
  | Initial (AnnModItem ann) !(Statement ann)
  | Always (AnnModItem ann) !(Statement ann)
  | Property
      { propAnnotation :: AnnModItem ann,
        propLabel :: {-# UNPACK #-} !Identifier,
        propEvent :: !(Event ann),
        propBodyL :: Maybe (Expr ann),
        propBodyR :: Expr ann
      }
  | Decl
      { declAnnotation :: AnnModItem ann,
        declDir :: !(Maybe PortDir),
        declPort :: !(Port ann),
        declVal :: Maybe (ConstExpr ann)
      }
  | ParamDecl (AnnModItem ann) (NonEmpty (Parameter ann))
  | LocalParamDecl (AnnModItem ann) (NonEmpty (LocalParam ann))
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (ModItem ann)

deriving instance AnnConstraint Show ann => Show (ModItem ann)

deriving instance AnnConstraint Ord ann => Ord (ModItem ann)

deriving instance Annotation ann => Data (ModItem ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (ModItem ann)

instance Annotated ModItem where
  setDefaultAnnotations (ModCA _ a) = ModCA def (setDefaultAnnotations a)
  setDefaultAnnotations (ModInst _ a b c d) = ModInst def a (setDefaultAnnotations <$> b) c (setDefaultAnnotations <$> d)
  setDefaultAnnotations (Initial _ a) = Initial def (setDefaultAnnotations a)
  setDefaultAnnotations (Always _ a) = Always def (setDefaultAnnotations a)
  setDefaultAnnotations (Property _ a b c d) = Property def a (setDefaultAnnotations b) (setDefaultAnnotations <$> c) (setDefaultAnnotations d)
  setDefaultAnnotations (Decl _ a b c) = Decl def a (setDefaultAnnotations b) (setDefaultAnnotations <$> c)
  setDefaultAnnotations (ParamDecl _ a) = ParamDecl def (setDefaultAnnotations <$> a)
  setDefaultAnnotations (LocalParamDecl _ a) = LocalParamDecl def (setDefaultAnnotations <$> a)

instance
  AnnModItem ann ~ annType =>
  HasField "annotation" (ModItem ann) annType
  where
  getField (ModCA ann _) = ann
  getField (ModInst ann _ _ _ _) = ann
  getField (Initial ann _) = ann
  getField (Always ann _) = ann
  getField (Property ann _ _ _ _) = ann
  getField (Decl ann _ _ _) = ann
  getField (ParamDecl ann _) = ann
  getField (LocalParamDecl ann _) = ann

makePrismLabels ''ModItem
makeFieldLabelsNoPrefix ''ModItem

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl ann = ModDecl
  { annotation :: AnnModDecl ann,
    id :: {-# UNPACK #-} !Identifier,
    outPorts :: ![Port ann],
    inPorts :: ![Port ann],
    items :: ![ModItem ann],
    params :: ![Parameter ann]
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (ModDecl ann)

deriving instance AnnConstraint Show ann => Show (ModDecl ann)

deriving instance AnnConstraint Ord ann => Ord (ModDecl ann)

deriving instance Annotation ann => Data (ModDecl ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (ModDecl ann)

instance Annotated ModDecl where
  setDefaultAnnotations (ModDecl _ a b c d e) =
    ModDecl
      def
      a
      (setDefaultAnnotations <$> b)
      (setDefaultAnnotations <$> c)
      (setDefaultAnnotations <$> d)
      (setDefaultAnnotations <$> e)

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
  deriving (Generic)
  deriving newtype (Semigroup, Monoid)

deriving instance AnnConstraint Eq ann => Eq (Verilog ann)

deriving instance AnnConstraint Show ann => Show (Verilog ann)

deriving instance AnnConstraint Ord ann => Ord (Verilog ann)

deriving instance Annotation ann => Data (Verilog ann)

deriving newtype instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (Verilog ann)

instance Annotated Verilog where
  setDefaultAnnotations (Verilog a) = Verilog (setDefaultAnnotations <$> a)

makePrismLabels ''Verilog

-- | Top level type which contains all the source code and associated
-- information.
data SourceInfo a = SourceInfo
  { top :: {-# UNPACK #-} !Text,
    src :: !(Verilog a)
  }
  deriving (Generic)

deriving instance AnnConstraint Eq ann => Eq (SourceInfo ann)

deriving instance AnnConstraint Show ann => Show (SourceInfo ann)

deriving instance AnnConstraint Ord ann => Ord (SourceInfo ann)

deriving instance Annotation ann => Data (SourceInfo ann)

deriving instance (AnnConstraint Generic ann, AnnConstraint NFData ann) => NFData (SourceInfo ann)

instance Annotated SourceInfo where
  setDefaultAnnotations (SourceInfo a b) = SourceInfo a (setDefaultAnnotations b)

makeFieldLabelsNoPrefix ''SourceInfo

instance Semigroup (SourceInfo a) where
  (SourceInfo t v) <> (SourceInfo _ v2) = SourceInfo t $ v <> v2

instance Monoid (SourceInfo ann) where
  mempty = SourceInfo mempty mempty

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

-- | Use this to change the name of the top module
topModuleId :: Lens' (SourceInfo a) Identifier
topModuleId = lens get_ set_
  where
    get_ src = Identifier src.top
    set_ src n =
      src
        & mainModule % #id .~ n
        & #top .~ n.getIdentifier

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
