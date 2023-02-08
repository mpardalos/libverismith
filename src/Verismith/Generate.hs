{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : Verismith.Generate
-- Description : Various useful generators.
-- Copyright   : (c) 2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Various useful generators.
module Verismith.Generate
  ( -- * Generation methods
    procedural,
    proceduralIO,
    proceduralSrc,
    proceduralSrcIO,
    randomMod,

    -- ** Data types
    EMIContext (..),
    Context (..),
    StateGen,
    ConfProperty (..),
    Config (..),
    ProbExpr (..),
    ProbMod (..),
    ProbModItem (..),
    ProbStatement (..),
    Probability (..),

    -- ** Generate Functions
    largeNum,
    wireSize,
    range,
    genBitVec,
    binOp,
    unOp,
    constExprWithContext,
    exprSafeList,
    exprRecList,
    exprWithContext,
    makeIdentifier,
    nextWirePort,
    nextNBPort,
    nextBPort,
    newWirePort,
    newNBPort,
    newBPort,
    scopedExpr,
    contAssign,
    lvalFromPort,
    assignment,
    seqBlock,
    conditional,
    forLoop,
    statement,
    alwaysSeq,
    instantiate,
    modInst,
    modItem,
    constExpr,
    parameter,
    moduleDef,

    -- ** Helpers
    someI,
    probability,
    askProbability,
    resizePort,
    moduleName,
    evalRange,
    calcRange,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (fold)
import Data.Functor.Foldable (cata)
import Data.List (foldl', partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hedgehog (Gen, GenT, MonadGen, Seed)
import Hedgehog qualified as Hog
import Hedgehog.Gen qualified as Hog
import Hedgehog.Range qualified as Hog
import Optics (toListOf, traversed, use, view, (%), (&), (.~), (^..))
import Optics.Operators.Unsafe ((^?!))
import Optics.State.Operators ((%=), (.=))
import Verismith.Internal
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.Eval
import Verismith.Verilog.Internal
import Verismith.Verilog.Mutate

-- | Probability of different expressions nodes.
data ProbExpr = ProbExpr
  { -- | @expr.number@: probability of generation a number like
    -- @4'ha@. This should never be set to 0, as it is used
    -- as a fallback in case there are no viable
    -- identifiers, such as none being in scope.
    num :: {-# UNPACK #-} !Int,
    -- | @expr.variable@: probability of generating an identifier that is in
    -- scope and of the right type.
    id :: {-# UNPACK #-} !Int,
    -- | @expr.rangeselect@: probability of generating a range
    -- selection from a port (@reg1[2:0]@).
    rangeSelect :: {-# UNPACK #-} !Int,
    -- | @expr.unary@: probability of generating a unary operator.
    unOp :: {-# UNPACK #-} !Int,
    -- | @expr.binary@: probability of generation a binary operator.
    binOp :: {-# UNPACK #-} !Int,
    -- | @expr.ternary@: probability of generating a conditional ternary
    -- operator.
    cond :: {-# UNPACK #-} !Int,
    -- | @expr.concatenation@: probability of generating a concatenation.
    concat :: {-# UNPACK #-} !Int,
    -- | @expr.string@: probability of generating a string. This is not
    -- fully supported therefore currently cannot be set.
    str :: {-# UNPACK #-} !Int,
    -- | @expr.signed@: probability of generating a signed function
    -- @$signed(...)@.
    signed :: {-# UNPACK #-} !Int,
    -- | @expr.unsigned@: probability of generating an unsigned function
    -- @$unsigned(...)@.
    unsigned :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show, Generic)

-- | Probability of generating different nodes inside a module declaration.
data ProbModItem = ProbModItem
  { -- | @moditem.assign@: probability of generating an @assign@.
    assign :: {-# UNPACK #-} !Int,
    -- | @moditem.sequential@: probability of generating a sequential @always@ block.
    seqAlways :: {-# UNPACK #-} !Int,
    -- | @moditem.combinational@: probability of generating an combinational @always@
    -- block. This is currently not implemented.
    combAlways :: {-# UNPACK #-} !Int,
    -- | @moditem.instantiation@: probability of generating a module
    -- instantiation.
    inst :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show, Generic)

-- | Probability of generating different statements.
data ProbStatement = ProbStatement
  { -- | @statement.blocking@: probability of generating blocking assignments.
    block :: {-# UNPACK #-} !Int,
    -- | @statement.nonblocking@: probability of generating nonblocking assignments.
    nonBlock :: {-# UNPACK #-} !Int,
    -- | @statement.conditional@: probability of generating conditional
    -- statements (@if@ statements).
    cond :: {-# UNPACK #-} !Int,
    -- | @statement.forloop@: probability of generating for loops.
    for :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show, Generic)

-- | Probability of generating various properties of a module.
data ProbMod = ProbMod
  { -- | "@module.drop_output@: frequency of a wire or register being dropped from the output."
    dropOutput :: {-# UNPACK #-} !Int,
    -- | "@module.keep_output@: frequency of a wire or register being kept in the output."
    keepOutput :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show, Generic)

-- | @[probability]@: combined probabilities.
data Probability = Probability
  { -- | Probabilities for module items.
    modItem :: {-# UNPACK #-} !ProbModItem,
    -- | Probabilities for statements.
    stmnt :: {-# UNPACK #-} !ProbStatement,
    -- | Probaiblities for expressions.
    expr :: {-# UNPACK #-} !ProbExpr,
    mod :: {-# UNPACK #-} !ProbMod
  }
  deriving (Eq, Show, Generic)

-- | @[property]@: properties for the generated Verilog file.
data ConfProperty = ConfProperty
  { -- | @size@: the size of the generated Verilog.
    size :: {-# UNPACK #-} !Int,
    -- | @seed@: a possible seed that could be used to
    -- generate the same Verilog.
    seed :: !(Maybe Seed),
    -- | @statement.depth@: the maximum statement depth that should be
    -- reached.
    stmntDepth :: {-# UNPACK #-} !Int,
    -- | @module.depth@: the maximium module depth that should be
    -- reached.
    modDepth :: {-# UNPACK #-} !Int,
    -- | @module.max@: the maximum number of modules that are
    -- allowed to be created at each level.
    maxModules :: {-# UNPACK #-} !Int,
    -- | @sample.method@: the sampling method that should be used to
    -- generate specific distributions of random
    -- programs.
    sampleMethod :: !Text,
    -- | @sample.size@: the number of samples to take for the
    -- sampling method.
    sampleSize :: {-# UNPACK #-} !Int,
    -- | @output.combine@: if the output should be combined into one
    -- bit or not.
    combine :: !Bool,
    -- | @nondeterminism@: the frequency at which nondeterminism
    -- should be generated (currently a work in progress).
    nonDeterminism :: {-# UNPACK #-} !Int,
    -- | @determinism@: the frequency at which determinism should
    -- be generated (currently modules are always deterministic).
    determinism :: {-# UNPACK #-} !Int,
    -- | @default.yosys@: Default location for Yosys, which will be used for
    -- equivalence checking.
    defaultYosys :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

data Config = Config
  { probability :: {-# UNPACK #-} !Probability,
    property :: {-# UNPACK #-} !ConfProperty
  }
  deriving (Eq, Show, Generic)

data EMIContext = EMIContext
  { _emiNewInputs :: [Port ()]
  }

data Context = Context
  { wires :: [Port ()],
    nonblocking :: [Port ()],
    blocking :: [Port ()],
    outofscope :: [Port ()],
    parameters :: [Parameter ()],
    modules :: [ModDecl ()],
    nameCounter :: {-# UNPACK #-} !Int,
    stmntDepth :: {-# UNPACK #-} !Int,
    modDepth :: {-# UNPACK #-} !Int,
    determinism :: !Bool,
    emiContext :: !(Maybe EMIContext)
  }
  deriving (Generic)

type StateGen = ReaderT Config (GenT (State Context))

toId :: Int -> Identifier
toId = Identifier . ("w" <>) . T.pack . show

toPort :: (MonadGen m) => Identifier -> m (Port ())
toPort ident = do
  i <- range
  return $ wire i ident

sumSize :: [Port ()] -> Range ()
sumSize ps = sum $ ps ^.. traversed % #size

random :: (MonadGen m) => [Port ()] -> (Expr () -> ContAssign ()) -> m (ModItem ())
random ctx fun = do
  expr <- Hog.sized (exprWithContext (ProbExpr 1 1 0 1 1 1 1 0 1 1) [] ctx)
  return . ModCA () $ fun expr

-- randomAssigns :: [Identifier] -> [Gen ModItem]
-- randomAssigns ids = random ids . ContAssign <$> ids

randomOrdAssigns :: (MonadGen m) => [Port ()] -> [Port ()] -> [m (ModItem ())]
randomOrdAssigns inp ids = snd $ foldr generate (inp, []) ids
  where
    generate cid (i, o) = (cid : i, random i (ContAssign cid.name) : o)

randomMod :: (MonadGen m) => Int -> Int -> m (ModDecl ())
randomMod inps total = do
  ident <- sequence $ toPort <$> ids
  x <- sequence $ randomOrdAssigns (start ident) (end ident)
  let inputs_ = take inps ident
  let other = drop inps ident
  let y = ModCA () . ContAssign "y" . fold $ Id () <$> drop inps ids
  let yport = [wire (sumSize other) "y"]
  return . declareMod other $
    ModDecl
      ()
      "test_module"
      yport
      inputs_
      (x ++ [y])
      []
  where
    ids = toId <$> [1 .. total]
    end = drop inps
    start = take inps

-- | Converts a 'Port' to an 'LVal' by only keeping the 'Identifier' of the
-- 'Port'.
lvalFromPort :: Port () -> LVal ()
lvalFromPort (Port _ _ _ i) = RegId i

-- | Returns the probability from the configuration.
probability :: Config -> Probability
probability c = c.probability

-- | Gets the current probabilities from the 'State'.
askProbability :: StateGen Probability
askProbability = asks probability

-- | Generates a random large number, which can also be negative.
largeNum :: (MonadGen m) => m Int
largeNum = Hog.int $ Hog.linear (-100) 100

-- | Generates a random size for a wire so that it is not too small and not too
-- large.
wireSize :: (MonadGen m) => m Int
wireSize = Hog.int $ Hog.linear 2 100

-- | Generates a random range by using the 'wireSize' and 0 as the lower bound.
range :: (MonadGen m) => m (Range ())
range = Range <$> fmap fromIntegral wireSize <*> pure 0

-- | Generate a random bit vector using 'largeNum'.
genBitVec :: (MonadGen m) => m BitVec
genBitVec = fmap fromIntegral largeNum

-- | Return a random 'BinaryOperator'. This currently excludes 'BinDiv',
-- 'BinMod' because they can take a long time to synthesis, and 'BinCEq',
-- 'BinCNEq', because these are not synthesisable. 'BinPower' is also excluded
-- because it can only be used in conjunction with base powers of 2 which is
-- currently not enforced.
binOp :: (MonadGen m) => m BinaryOperator
binOp =
  Hog.element
    [ BinPlus,
      BinMinus,
      BinTimes,
      -- , BinDiv
      -- , BinMod
      BinEq,
      BinNEq,
      -- , BinCEq
      -- , BinCNEq
      BinLAnd,
      BinLOr,
      BinLT,
      BinLEq,
      BinGT,
      BinGEq,
      BinAnd,
      BinOr,
      BinXor,
      BinXNor,
      BinXNorInv,
      -- , BinPower
      BinLSL,
      BinLSR,
      BinASL,
      BinASR
    ]

-- | Generate a random 'UnaryOperator'.
unOp :: (MonadGen m) => m UnaryOperator
unOp =
  Hog.element
    [ UnPlus,
      UnMinus,
      UnNot,
      UnLNot,
      UnAnd,
      UnNand,
      UnOr,
      UnNor,
      UnXor,
      UnNxor,
      UnNxorInv
    ]

-- | Generate a random 'ConstExpr' by using the current context of 'Parameter'.
constExprWithContext :: (MonadGen m) => [Parameter ()] -> ProbExpr -> Hog.Size -> m (ConstExpr ())
constExprWithContext ps prob size
  | size == 0 =
      Hog.frequency
        [ (prob.num, ConstNum () <$> genBitVec),
          ( if null ps then 0 else prob.id,
            ParamId () . view #ident <$> Hog.element ps
          )
        ]
  | size > 0 =
      Hog.frequency
        [ (prob.num, ConstNum () <$> genBitVec),
          ( if null ps then 0 else prob.id,
            ParamId () . view #ident <$> Hog.element ps
          ),
          (prob.unOp, ConstUnOp () <$> unOp <*> subexpr 2),
          ( prob.binOp,
            ConstBinOp () <$> subexpr 2 <*> binOp <*> subexpr 2
          ),
          ( prob.cond,
            ConstCond () <$> subexpr 2 <*> subexpr 2 <*> subexpr 2
          ),
          ( prob.concat,
            ConstConcat () <$> Hog.nonEmpty (Hog.linear 0 10) (subexpr 2)
          )
        ]
  | otherwise = constExprWithContext ps prob 0
  where
    subexpr y = constExprWithContext ps prob $ size `div` y

-- | The list of safe 'Expr', meaning that these will not recurse and will end
-- the 'Expr' generation.
exprSafeList :: (MonadGen m) => ProbExpr -> [(Int, m (Expr ()))]
exprSafeList prob = [(prob.num, Number () <$> genBitVec)]

-- | List of 'Expr' that have the chance to recurse and will therefore not be
-- used when the expression grows too large.
exprRecList :: (MonadGen m) => ProbExpr -> (Hog.Size -> m (Expr ())) -> [(Int, m (Expr ()))]
exprRecList prob subexpr =
  [ (prob.num, Number () <$> genBitVec),
    ( prob.concat,
      Concat () <$> Hog.nonEmpty (Hog.linear 0 10) (subexpr 2)
    ),
    (prob.unOp, UnOp () <$> unOp <*> subexpr 2),
    (prob.str, Str () <$> Hog.text (Hog.linear 0 100) Hog.alphaNum),
    (prob.binOp, BinOp () <$> subexpr 2 <*> binOp <*> subexpr 2),
    (prob.cond, Cond () <$> subexpr 2 <*> subexpr 2 <*> subexpr 2),
    (prob.signed, Appl () <$> pure "$signed" <*> subexpr 2),
    (prob.unsigned, Appl () <$> pure "$unsigned" <*> subexpr 2)
  ]

-- | Select a random port from a list of ports and generate a safe bit selection
-- for that port.
rangeSelect :: (MonadGen m) => [Parameter ()] -> [Port ()] -> m (Expr ())
rangeSelect ps ports = do
  p <- Hog.element ports
  let s = calcRange ps (Just 32) $ p.size
  msb <- Hog.int (Hog.constantFrom (s `div` 2) 0 (s - 1))
  lsb <- Hog.int (Hog.constantFrom (msb `div` 2) 0 msb)
  return $
    RangeSelect () p.name $
      Range
        (fromIntegral msb)
        (fromIntegral lsb)

-- | Generate a random expression from the 'Context' with a guarantee that it
-- will terminate using the list of safe 'Expr'.
exprWithContext :: (MonadGen m) => ProbExpr -> [Parameter ()] -> [Port ()] -> Hog.Size -> m (Expr ())
exprWithContext prob ps [] n
  | n == 0 = Hog.frequency $ exprSafeList prob
  | n > 0 = Hog.frequency $ exprRecList prob subexpr
  | otherwise = exprWithContext prob ps [] 0
  where
    subexpr y = exprWithContext prob ps [] $ n `div` y
exprWithContext prob ps l n
  | n == 0 =
      Hog.frequency $
        (prob.id, Id () . fromPort <$> Hog.element l)
          : exprSafeList prob
  | n > 0 =
      Hog.frequency $
        (prob.id, Id () . fromPort <$> Hog.element l)
          : (prob.rangeSelect, rangeSelect ps l)
          : exprRecList prob subexpr
  | otherwise =
      exprWithContext prob ps l 0
  where
    subexpr y = exprWithContext prob ps l $ n `div` y

-- | Runs a 'StateGen' for a random number of times, limited by an 'Int' that is
-- passed to it.
someI :: Int -> StateGen a -> StateGen [a]
someI m f = do
  amount <- Hog.int (Hog.linear 1 m)
  replicateM amount f

-- | Make a new name with a prefix and the current nameCounter. The nameCounter
-- is then increased so that the label is unique.
makeIdentifier :: Text -> StateGen Identifier
makeIdentifier prefix = do
  context <- get
  let ident = Identifier $ prefix <> showT (context.nameCounter)
  #nameCounter %= (+ 1)
  return ident

newPort_ :: Bool -> PortType -> Identifier -> StateGen (Port ())
newPort_ blk pt ident = do
  p <- Port pt <$> Hog.bool <*> range <*> pure ident
  case pt of
    Reg -> if blk then #blocking %= (p :) else #nonblocking %= (p :)
    Wire -> #wires %= (p :)
  return p

-- | Creates a new port based on the current name counter and adds it to the
-- current context.  It will be added to the '_wires' list.
newWirePort :: Identifier -> StateGen (Port ())
newWirePort = newPort_ False Wire

-- | Creates a new port based on the current name counter and adds it to the
-- current context.  It will be added to the '_nonblocking' list.
newNBPort :: Identifier -> StateGen (Port ())
newNBPort = newPort_ False Reg

-- | Creates a new port based on the current name counter and adds it to the
-- current context.  It will be added to the '_blocking' list.
newBPort :: Identifier -> StateGen (Port ())
newBPort = newPort_ True Reg

getPort' :: Bool -> PortType -> Identifier -> StateGen (Maybe (Port ()))
getPort' blk pt i = do
  cont <- get
  let b = cont.blocking
  let nb = cont.nonblocking
  let w = cont.wires
  let (c, nc) =
        case pt of
          Reg -> if blk then (b, nb <> w) else (nb, b <> w)
          Wire -> (w, b <> nb)
  case (filter portId c, filter portId nc) of
    (_, _ : _) -> return Nothing
    (x : _, []) -> return $ Just x
    ([], []) ->
      fmap
        Just
        ( case pt of
            Reg -> if blk then newBPort i else newNBPort i
            Wire -> newWirePort i
        )
  where
    portId (Port pt' _ _ i') = i == i' && pt == pt'

try :: StateGen (Maybe a) -> StateGen a
try a = do
  r <- a
  case r of
    Nothing -> try a
    Just res -> return res

-- | Makes a new 'Identifier' and then checks if the 'Port' already exists, if
-- it does the existant 'Port' is returned, otherwise a new port is created with
-- 'newPort'. This is used subsequently in all the functions to create a port,
-- in case a port with the same name was already created. This could be because
-- the generation is currently in the other branch of an if-statement.
nextWirePort :: Maybe Text -> StateGen (Port ())
nextWirePort i = try $ do
  ident <- makeIdentifier $ fromMaybe (T.toLower $ showT Wire) i
  getPort' False Wire ident

nextNBPort :: Maybe Text -> StateGen (Port ())
nextNBPort i = try $ do
  ident <- makeIdentifier $ fromMaybe (T.toLower $ showT Reg) i
  getPort' False Reg ident

nextBPort :: Maybe Text -> StateGen (Port ())
nextBPort i = try $ do
  ident <- makeIdentifier $ fromMaybe (T.toLower $ showT Reg) i
  getPort' True Reg ident

allVariables :: StateGen [Port ()]
allVariables =
  fmap (\context -> context.wires <> context.nonblocking <> context.blocking) get

shareableVariables :: StateGen [Port ()]
shareableVariables =
  fmap (\context -> context.wires <> context.nonblocking) get

-- | Generates an expression from variables that are currently in scope.
scopedExpr_ :: [Port ()] -> StateGen (Expr ())
scopedExpr_ vars = do
  context <- get
  prob <- askProbability
  Hog.sized
    . exprWithContext prob.expr context.parameters
    $ vars

scopedExprAll :: StateGen (Expr ())
scopedExprAll = allVariables >>= scopedExpr_

scopedExpr :: StateGen (Expr ())
scopedExpr = shareableVariables >>= scopedExpr_

-- | Generates a random continuous assignment and assigns it to a random wire
-- that is created.
contAssign :: StateGen (ContAssign ())
contAssign = do
  expr <- scopedExpr
  p <- nextWirePort Nothing
  return $ ContAssign p.name expr

-- | Generate a random assignment and assign it to a random 'Reg'.
assignment :: Bool -> StateGen (Assign ())
assignment blk = do
  expr <- scopedExprAll
  lval <- lvalFromPort <$> (if blk then nextBPort else nextNBPort) Nothing
  return $ Assign lval Nothing expr

-- | Generate a random 'Statement' safely, by also increasing the depth counter.
seqBlock :: StateGen (Statement ())
seqBlock = do
  #stmntDepth %= (+ 1)
  tstat <- SeqBlock () <$> someI 20 statement
  #stmntDepth %= (+ 1)
  return tstat

-- | Generate a random conditional 'Statement'. The nameCounter is reset between
-- branches so that port names can be reused. This is safe because if a 'Port'
-- is not reused, it is left at 0, as all the 'Reg' are initialised to 0 at the
-- start.
conditional :: StateGen (Statement ())
conditional = do
  expr <- scopedExprAll
  nc <- use #nameCounter
  tstat <- seqBlock
  nc' <- use #nameCounter
  #nameCounter .= nc
  fstat <- seqBlock
  nc'' <- use #nameCounter
  #nameCounter .= max nc' nc''
  return $ CondStmnt () expr (Just tstat) (Just fstat)

-- | Generate a random for loop by creating a new variable name for the counter
-- and then generating random statements in the body.
forLoop :: StateGen (Statement ())
forLoop = do
  num <- Hog.int (Hog.linear 0 20)
  var <- lvalFromPort <$> nextBPort (Just "forvar")
  ForLoop
    ()
    (Assign var Nothing 0)
    (BinOp () (varId var) BinLT $ fromIntegral num)
    (Assign var Nothing $ BinOp () (varId var) BinPlus 1)
    <$> seqBlock
  where
    varId v = Id () (v ^?! #_RegId)

-- | Choose a 'Statement' to generate.
statement :: StateGen (Statement ())
statement = do
  prob <- askProbability
  cont <- get
  Hog.frequency
    [ (prob.stmnt.block, BlockAssign () <$> assignment True),
      (prob.stmnt.nonBlock, NonBlockAssign () <$> assignment False),
      (onDepth cont prob.stmnt.cond, conditional),
      (onDepth cont prob.stmnt.for, forLoop)
    ]
  where
    onDepth c n = if c.stmntDepth > 0 then n else 0

-- | Generate a sequential always block which is dependent on the clock.
alwaysSeq :: StateGen (ModItem ())
alwaysSeq = do
  always <- Always () . EventCtrl () (EPosEdge "clk") . Just <$> seqBlock
  blk <- use #blocking
  #outofscope %= mappend blk
  #blocking .= []
  return always

-- | Should resize a port that connects to a module port if the latter is
-- larger.  This should not cause any problems if the same net is used as input
-- multiple times, and is resized multiple times, as it should only get larger.
resizePort :: [Parameter ()] -> Identifier -> Range () -> [Port ()] -> [Port ()]
resizePort ps i ra = foldl' func []
  where
    func l p@(Port _ _ ri i')
      | i' == i && calc ri < calc ra = (p & #size .~ ra) : l
      | otherwise = p : l
    calc = calcRange ps $ Just 64

-- | Instantiate a module, where the outputs are new nets that are created, and
-- the inputs are taken from existing ports in the context.
--
-- 1 is subtracted from the inputs for the length because the clock is not
-- counted and is assumed to be there, this should be made nicer by filtering
-- out the clock instead. I think that in general there should be a special
-- representation for the clock.
instantiate :: ModDecl () -> StateGen (ModItem ())
instantiate (ModDecl () i outP inP _ _) = do
  vars <- shareableVariables
  outs <- replicateM (length outP) $ nextWirePort Nothing
  ins <- take (length inpFixed) <$> Hog.shuffle vars
  insLit <- replicateM (length inpFixed - length ins) (Number () <$> genBitVec)
  mapM_ (uncurry process)
    . zip
      ( zip
          (ins ^.. traversed % #name)
          (ins ^.. traversed % #portType)
      )
    $ inpFixed ^.. traversed % #size
  ident <- makeIdentifier "modinst"
  Hog.choice
    [ return . ModInst () i [] ident $ ModConn <$> (toE (outs <> clkPort <> ins) <> insLit),
      ModInst () i [] ident
        <$> Hog.shuffle
          ( zipWith
              ModConnNamed
              (view #name <$> outP <> clkPort <> inpFixed)
              (toE (outs <> clkPort <> ins) <> insLit)
          )
    ]
  where
    toE ins = Id () . view #name <$> ins
    (inpFixed, clkPort) = partition filterFunc inP
    filterFunc (Port _ _ _ n)
      | n == "clk" = False
      | otherwise = True
    process (p, t) r = do
      params <- use #parameters
      case t of
        Reg -> #nonblocking %= resizePort params p r
        Wire -> #wires %= resizePort params p r

-- | Generates a module instance by also generating a new module if there are
-- not enough modules currently in the context. It keeps generating new modules
-- for every instance and for every level until either the deepest level is
-- achieved, or the maximum number of modules are reached.
--
-- If the maximum number of levels are reached, it will always pick an instance
-- from the current context. The problem with this approach is that at the end
-- there may be many more than the max amount of modules, as the modules are
-- always set to empty when entering a new level. This is to fix recursive
-- definitions of modules, which are not defined.
--
-- One way to fix that is to also decrement the max modules for every level,
-- depending on how many modules have already been generated. This would mean
-- there would be moments when the module cannot generate a new instance but
-- also not take a module from the current context. A fix for that may be to
-- have a default definition of a simple module that is used instead.
--
-- Another different way to handle this would be to have a probability of taking
-- a module from a context or generating a new one.
modInst :: StateGen (ModItem ())
modInst = do
  prob <- ask
  context <- get
  let maxMods = prob.property.maxModules
  if length (context.modules) < maxMods
    then do
      let currMods = context.modules
      let params = context.parameters
      let w = context.wires
      let nb = context.nonblocking
      let b = context.blocking
      let oos = context.outofscope
      #modules .= []
      #wires .= []
      #nonblocking .= []
      #blocking .= []
      #outofscope .= []
      #parameters .= []
      #modDepth %= subtract 1
      chosenMod <- moduleDef Nothing
      ncont <- get
      let genMods = ncont.modules
      #modDepth %= (1 +)
      #parameters .= params
      #wires .= w
      #nonblocking .= nb
      #blocking .= b
      #outofscope .= oos
      #modules .= chosenMod : currMods <> genMods
      instantiate chosenMod
    else Hog.element (context.modules) >>= instantiate

-- | Generate a random module item.
modItem :: StateGen (ModItem ())
modItem = do
  conf <- ask
  context <- get
  det <-
    Hog.frequency
      [ (conf.property.determinism, return True),
        (conf.property.nonDeterminism, return False)
      ]
  #determinism .= det
  Hog.frequency
    [ (conf.probability.modItem.assign, ModCA () <$> contAssign),
      (conf.probability.modItem.seqAlways, alwaysSeq),
      ( if context.modDepth > 0 then conf.probability.modItem.inst else 0,
        modInst
      )
    ]

-- | Either return the 'Identifier' that was passed to it, or generate a new
-- 'Identifier' based on the current 'nameCounter'.
moduleName :: Maybe Identifier -> StateGen Identifier
moduleName (Just t) = return t
moduleName Nothing = makeIdentifier "module"

-- | Generate a random 'ConstExpr' by using the current context of 'Parameters'.
constExpr :: StateGen (ConstExpr ())
constExpr = do
  prob <- askProbability
  context <- get
  Hog.sized $
    constExprWithContext
      (context.parameters)
      (prob.expr)

-- | Generate a random 'Parameter' and assign it to a constant expression which
-- it will be initialised to. The assumption is that this constant expression
-- should always be able to be evaluated with the current context of parameters.
parameter :: StateGen (Parameter ())
parameter = do
  ident <- makeIdentifier "param"
  cexpr <- constExpr
  let param = Parameter ident cexpr
  #parameters %= (param :)
  return param

-- | Evaluate a range to an integer, and cast it back to a range.
evalRange :: [Parameter ()] -> Int -> Range () -> Range ()
evalRange ps n (Range l r) = Range (eval l) (eval r)
  where
    eval = ConstNum () . cata (evaluateConst ps) . resize n

-- | Calculate a range to an int by maybe resizing the ranges to a value.
calcRange :: [Parameter ()] -> Maybe Int -> Range () -> Int
calcRange ps i (Range l r) = eval l - eval r + 1
  where
    eval a = fromIntegral . cata (evaluateConst ps) $ maybe a (`resize` a) i

-- | Filter out a port based on it's name instead of equality of the ports. This
-- is because the ports might not be equal if the sizes are being updated.
identElem :: Port () -> [Port ()] -> Bool
identElem p = elem p.name . toListOf (traversed % #name)

-- | Select items from a list with a specific frequency, returning the new list
-- that contains the selected items. If 0 is passed to both the select and
-- not-select parameter, the function will act like the idententy, returning the
-- original list inside the 'Gen' monad.
--
-- The reason for doing this at the output of a module reduces the number of
-- wires that are exposed at the output and therefore allows the synthesis tool
-- to perform more optimisations that it could otherwise not perform. The
-- synthesis tool is quite strict with optimisations if all the wires and
-- registers are exposed.
selectwfreq :: (MonadGen m) => Int -> Int -> [a] -> m [a]
selectwfreq _ _ [] = return []
selectwfreq s n a@(l : ls)
  | s > 0 && n > 0 =
      Hog.frequency
        [ (s, (l :) <$> selectwfreq s n ls),
          (n, selectwfreq s n ls)
        ]
  | otherwise = return a

-- | Generates a module definition randomly. It always has one output port which
-- is set to @y@. The size of @y@ is the total combination of all the locally
-- defined wires, so that it correctly reflects the internal state of the
-- module.
moduleDef :: Maybe Identifier -> StateGen (ModDecl ())
moduleDef top = do
  name <- moduleName top
  portList <- Hog.list (Hog.linear 4 10) $ nextWirePort Nothing
  mi <- Hog.list (Hog.linear 4 100) modItem
  ps <- Hog.list (Hog.linear 0 10) parameter
  context <- get
  vars <- shareableVariables
  config <- ask
  let (newPorts, localPorts) = partition (`identElem` portList) $ vars <> context.outofscope
  let size =
        evalRange context.parameters 32
          . sum
          $ localPorts ^.. traversed % #size
  let (ProbMod n s) = config.probability.mod
  newlocal <- selectwfreq s n localPorts
  let clock = Port Wire False 1 "clk"
  let combine = config.property.combine
  let yport =
        if combine then Port Wire False 1 "y" else Port Wire False size "y"
  let comb = combineAssigns_ combine yport newlocal
  return
    . declareMod localPorts
    . ModDecl () name [yport] (clock : newPorts) (comb : mi)
    $ ps

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
procedural :: Text -> Config -> Gen (Verilog ())
procedural top config = do
  (mainMod, st) <-
    Hog.resize num $
      runStateT
        (Hog.distributeT (runReaderT (moduleDef (Just $ Identifier top)) config))
        context
  return . Verilog $ mainMod : st.modules
  where
    context =
      Context [] [] [] [] [] [] 0 config.property.stmntDepth config.property.modDepth True Nothing
    num = fromIntegral $ config.property.size

-- | Samples the 'Gen' directly to generate random 'Verilog' using the 'Text' as
-- the name of the main module and the configuration 'Config' to influence the
-- generation.
proceduralIO :: Text -> Config -> IO (Verilog ())
proceduralIO t = Hog.sample . procedural t

-- | Given a 'Text' and a 'Config' will generate a '(SourceInfo ann)' which has the
-- top module set to the right name.
proceduralSrc :: Text -> Config -> Gen (SourceInfo ())
proceduralSrc t c = SourceInfo t <$> procedural t c

-- | Sampled and wrapped into a '(SourceInfo ann)' with the given top module name.
proceduralSrcIO :: Text -> Config -> IO (SourceInfo ())
proceduralSrcIO t c = SourceInfo t <$> proceduralIO t c
