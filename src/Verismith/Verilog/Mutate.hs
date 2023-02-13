{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Verismith.Verilog.Mutate
-- Description : Functions to mutate the Verilog AST.
-- Copyright   : (c) 2018-2022, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions to mutate the Verilog AST from "Verismith.Verilog.AST" to generate more
-- random patterns, such as nesting wires instead of creating new ones.
module Verismith.Verilog.Mutate
  ( mutExpr,
    inPort,
    findAssign,
    idTrans,
    replace,
    nestId,
    nestSource,
    nestUpTo,
    allVars,
    instantiateMod,
    instantiateMod_,
    instantiateModSpec_,
    filterChar,
    initMod,
    makeIdFrom,
    makeTop,
    makeTopAssert,
    simplify,
    removeId,
    combineAssigns,
    combineAssigns_,
    declareMod,
    fromPort,
  )
where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformBi)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Optics (traversed, (%), (%~), (&), (.~), (^..), _2)
import Optics.Operators.Unsafe ((^?!))
import Verismith.Circuit.Internal
import Verismith.Internal
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.Internal

mutExpr :: (Annotation ann, Data a) => (Expr ann -> Expr ann) -> a -> a
mutExpr = transformBi

-- | Return if the 'Identifier' is in a '(ModDecl ann)'.
inPort :: Identifier -> ModDecl ann -> Bool
inPort i m = inInput
  where
    inInput =
      any (\a -> a.name == i) $ m.inPorts ++ m.outPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModItem ann] -> Maybe (Expr ann)
findAssign i items = safe last (mapMaybe isAssign items)
  where
    isAssign (ModCA _ (ContAssign val expr))
      | val == i = Just expr
      | otherwise = Nothing
    isAssign _ = Nothing

-- | Transforms an expression by replacing an Identifier with an
-- expression. This is used inside 'transformOf' and 'traverseExpr' to replace
-- the 'Identifier' recursively.
idTrans :: Identifier -> Expr ann -> Expr ann -> Expr ann
idTrans i expr (Id ann id')
  | id' == i = expr
  | otherwise = Id ann id'
idTrans _ _ e = e

-- | Replaces the identifier recursively in an expression.
replace :: Annotation ann => Identifier -> Expr ann -> Expr ann -> Expr ann
replace ident e = transform (idTrans ident e)

-- | Nest expressions for a specific 'Identifier'. If the 'Identifier' is not
-- found, the AST is not changed.
--
-- This could be improved by instead of only using the last assignment to the
-- wire that one finds, to use the assignment to the wire before the current
-- expression. This would require a different approach though.
nestId :: Annotation ann => Identifier -> ModDecl ann -> ModDecl ann
nestId i m
  | not $ inPort i m =
      let expr = fromMaybe (Id def i) . findAssign i $ m.items
       in m & get %~ replace i expr
  | otherwise =
      m
  where
    get = #items % traversed % #_ModCA % _2 % #expr

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Annotation ann => Identifier -> Verilog ann -> Verilog ann
nestSource i src = src & getModule %~ nestId i

-- | Nest variables in the format @w[0-9]*@ up to a certain number.
nestUpTo :: Annotation ann => Int -> Verilog ann -> Verilog ann
nestUpTo i src =
  foldl (flip nestSource) src $ Identifier . fromNode <$> [1 .. i]

allVars :: ModDecl ann -> [Identifier]
allVars m =
  (m ^.. #outPorts % traversed % #name)
    <> (m ^.. #inPorts % traversed % #name)

-- $setup
-- >>> import Verismith.Verilog.CodeGen
-- >>> let m = (ModDecl (Identifier "m") [Port Wire False 5 (Identifier "y")] [Port Wire False 5 "x"] [] [])
-- >>> let main = (ModDecl "main" [] [] [] [])

-- | Add a Module Instantiation using 'ModInst' from the first module passed to
-- it to the body of the second module. It first has to make all the inputs into
-- @reg@.
--
-- >>> render $ instantiateMod m main
-- module main;
--   wire [(3'h4):(1'h0)] y;
--   reg [(3'h4):(1'h0)] x;
--   m m1(y, x);
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
instantiateMod :: Annotation ann => ModDecl ann -> ModDecl ann -> ModDecl ann
instantiateMod m main = main & #items %~ ((out ++ regIn ++ [inst]) ++)
  where
    out = Decl def Nothing <$> m.outPorts <*> pure Nothing
    regIn =
      Decl def Nothing
        <$> (m.inPorts & traversed % #portType .~ Reg)
        <*> pure Nothing
    inst =
      ModInst
        def
        (m ^?! #id)
        []
        (m ^?! #id <> (Identifier . showT $ count + 1))
        conns
    count =
      length
        . filter (== m ^?! #id)
        $ main ^.. #items % traversed % #instId
    conns = uncurry ModConnNamed . fmap (Id def) <$> zip (allVars m) (allVars m)

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> GenVerilog $ instantiateMod_ m
-- m m(y, x);
-- <BLANKLINE>
instantiateMod_ :: Annotation ann => ModDecl ann -> ModItem ann
instantiateMod_ m = ModInst def (m ^?! #id) [] (m ^?! #id) conns
  where
    conns =
      ModConn
        . Id def
        <$> (m ^.. #outPorts % traversed % #name)
          ++ (m ^.. #inPorts % traversed % #name)

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> GenVerilog $ instantiateModSpec_ "_" m
-- m m(.y(y), .x(x));
-- <BLANKLINE>
instantiateModSpec_ :: Annotation ann => Bool -> Text -> ModDecl ann -> ModItem ann
instantiateModSpec_ named outChar m = ModInst def (m ^?! #id) [] (m ^?! #id) conns
  where
    conns = (if named then zipWith ModConnNamed ids else map ModConn) (Id def <$> instIds)
    ids = filterChar outChar (name #outPorts) <> name #inPorts
    instIds = name #outPorts <> name #inPorts
    name v = m ^.. v % traversed % #name

filterChar :: Text -> [Identifier] -> [Identifier]
filterChar t ids =
  ids & traversed % #_Identifier %~ (\x -> fromMaybe x . safe head $ T.splitOn t x)

-- | Initialise all the inputs and outputs to a module.
--
-- >>> GenVerilog $ initMod m
-- module m(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
initMod :: Annotation ann => ModDecl ann -> ModDecl ann
initMod m = m & #items %~ ((out ++ inp) ++)
  where
    out = Decl def (Just PortOut) <$> m.outPorts <*> pure Nothing
    inp = Decl def (Just PortIn) <$> m.inPorts <*> pure Nothing

-- | Make an 'Identifier' from and existing Identifier and an object with a
-- 'Show' instance to make it unique.
makeIdFrom :: (Show a) => a -> Identifier -> Identifier
makeIdFrom a i = (i <>) . Identifier . ("_" <>) $ showT a

-- | Make top level module for equivalence verification. Also takes in how many
-- modules to instantiate.
makeTop :: Annotation ann => Bool -> Int -> ModDecl ann -> ModDecl ann
makeTop named i m = ModDecl def m.id ys m.inPorts modIt []
  where
    ys = yPort . flip makeIdFrom "y" <$> [1 .. i]
    modIt = instantiateModSpec_ named "_" . modN <$> [1 .. i]
    modN n =
      m & #id %~ makeIdFrom n & #outPorts .~ [yPort (makeIdFrom n "y")]

-- | Make a top module with an assert that requires @y_1@ to always be equal to
-- @y_2@, which can then be proven using a formal verification tool.
makeTopAssert :: Annotation ann => ModDecl ann -> ModDecl ann
makeTopAssert = (#items %~ (++ [assert])) . makeTop False 2
  where
    assert =
      Always def . EventCtrl def e . Just $
        SeqBlock
          def
          [TaskEnable def $ Task "assert" [BinOp def (Id def "y_1") BinEq (Id def "y_2")]]
    e = EPosEdge "clk"

-- | Provide declarations for all the ports that are passed to it. If they are
-- registers, it should assign them to 0.
declareMod :: Annotation ann => [Port ann] -> ModDecl ann -> ModDecl ann
declareMod ports = initMod . (#items %~ (fmap decl ports ++))
  where
    decl p@(Port Reg _ _ _) = Decl def Nothing p (Just (ConstNum def 0))
    decl p = Decl def Nothing p Nothing

-- | Simplify an 'Expr' by using constants to remove 'BinaryOperator' and
-- simplify expressions. To make this work effectively, it should be run until
-- no more changes were made to the expression.
--
-- >>> GenVerilog . simplify $ (Id "x") + 0
-- x
--
-- >>> GenVerilog . simplify $ (Id "y") + (Id "x")
-- (y + x)
simplify :: Annotation ann => Expr ann -> Expr ann
simplify (BinOp _ (Number _ (BitVec _ 1)) BinAnd e) = e
simplify (BinOp _ e BinAnd (Number _ (BitVec _ 1))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinAnd _) = Number def 0
simplify (BinOp _ _ BinAnd (Number _ (BitVec _ 0))) = Number def 0
simplify (BinOp _ e BinPlus (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinPlus e) = e
simplify (BinOp _ e BinMinus (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinMinus e) = e
simplify (BinOp _ e BinTimes (Number _ (BitVec _ 1))) = e
simplify (BinOp _ (Number _ (BitVec _ 1)) BinTimes e) = e
simplify (BinOp _ _ BinTimes (Number _ (BitVec _ 0))) = Number def 0
simplify (BinOp _ (Number _ (BitVec _ 0)) BinTimes _) = Number def 0
simplify (BinOp _ e BinOr (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinOr e) = e
simplify (BinOp _ e BinLSL (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinLSL e) = e
simplify (BinOp _ e BinLSR (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinLSR e) = e
simplify (BinOp _ e BinASL (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinASL e) = e
simplify (BinOp _ e BinASR (Number _ (BitVec _ 0))) = e
simplify (BinOp _ (Number _ (BitVec _ 0)) BinASR e) = e
simplify (UnOp _ UnPlus e) = e
simplify e = e

-- | Remove all 'Identifier' that do not appeare in the input list from an
-- 'Expr'. The identifier will be replaced by @1'b0@, which can then later be
-- simplified further.
--
-- >>> GenVerilog . removeId ["x"] $ Id "x" + Id "y"
-- (x + (1'h0))
removeId :: Annotation ann => [Identifier] -> Expr ann -> Expr ann
removeId i = transform trans
  where
    trans (Id ann ident)
      | ident `notElem` i = Number def 0
      | otherwise = Id ann ident
    trans e = e

combineAssigns :: (Annotation ann, Semigroup (AnnExpr ann)) => Port ann -> [ModItem ann] -> [ModItem ann]
combineAssigns p a =
  a
    <> [ ModCA def
           . ContAssign p.name
           . UnOp def UnXor
           $ foldMap (Id def) assigns
       ]
  where
    assigns = a ^.. traversed % #_ModCA % _2 % #lval

combineAssigns_ :: (Annotation ann, Semigroup (AnnExpr ann)) => Bool -> Port ann -> [Port ann] -> ModItem ann
combineAssigns_ comb p ps =
  ModCA def
    . ContAssign p.name
    . (if comb then UnOp def UnXor else id)
    $ foldMap (Id def) (ps ^.. traversed % #name)

fromPort :: Port ann -> Identifier
fromPort (Port _ _ _ i) = i
