-- |
-- Module      : Verismith.Verilog.Eval
-- Description : Evaluation of Verilog expressions and statements.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Evaluation of Verilog expressions and statements.
module Verismith.Verilog.Eval
  ( evaluateConst,
    resize,
  )
where

import Data.Bits
import Data.Foldable (fold, find)
import Data.Functor.Foldable hiding (fold)
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Data.Generics.Uniplate.Data (transformBi)

type Bindings ann = [Parameter ann]

paramIdent_ :: Parameter ann -> Identifier
paramIdent_ (Parameter i _) = i

paramValue_ :: Parameter ann -> ConstExpr ann
paramValue_ (Parameter _ v) = v

applyUnary :: (Num a, FiniteBits a) => UnaryOperator -> a -> a
applyUnary UnPlus a = a
applyUnary UnMinus a = negate a
applyUnary UnLNot a
  | a == 0 = 0
  | otherwise = 1
applyUnary UnNot a = complement a
applyUnary UnAnd a
  | finiteBitSize a == popCount a = 1
  | otherwise = 0
applyUnary UnNand a
  | finiteBitSize a == popCount a = 0
  | otherwise = 1
applyUnary UnOr a
  | popCount a == 0 = 0
  | otherwise = 1
applyUnary UnNor a
  | popCount a == 0 = 1
  | otherwise = 0
applyUnary UnXor a
  | popCount a `mod` 2 == 0 = 0
  | otherwise = 1
applyUnary UnNxor a
  | popCount a `mod` 2 == 0 = 1
  | otherwise = 0
applyUnary UnNxorInv a
  | popCount a `mod` 2 == 0 = 1
  | otherwise = 0

compXor :: Bits c => c -> c -> c
compXor a = complement . xor a

toIntegral :: Num p => (t1 -> t2 -> Bool) -> t1 -> t2 -> p
toIntegral a b c = if a b c then 1 else 0

toInt :: (Integral a, Num t1) => (t2 -> t1 -> t3) -> t2 -> a -> t3
toInt a b c = a b $ fromIntegral c

applyBinary :: (Integral a, Bits a) => BinaryOperator -> a -> a -> a
applyBinary BinPlus = (+)
applyBinary BinMinus = (-)
applyBinary BinTimes = (*)
applyBinary BinDiv = quot
applyBinary BinMod = rem
applyBinary BinEq = toIntegral (==)
applyBinary BinNEq = toIntegral (/=)
applyBinary BinCEq = toIntegral (==)
applyBinary BinCNEq = toIntegral (/=)
applyBinary BinLAnd = undefined
applyBinary BinLOr = undefined
applyBinary BinLT = toIntegral (<)
applyBinary BinLEq = toIntegral (<=)
applyBinary BinGT = toIntegral (>)
applyBinary BinGEq = toIntegral (>=)
applyBinary BinAnd = (.&.)
applyBinary BinOr = (.|.)
applyBinary BinXor = xor
applyBinary BinXNor = compXor
applyBinary BinXNorInv = compXor
applyBinary BinPower = undefined
applyBinary BinLSL = toInt shiftL
applyBinary BinLSR = toInt shiftR
applyBinary BinASL = toInt shiftL
applyBinary BinASR = toInt shiftR

-- | Evaluates a 'ConstExpr' using a context of 'Bindings' as input.
evaluateConst :: Bindings ann -> ConstExprF ann BitVec -> BitVec
evaluateConst _ (ConstNumF _ b) = b
evaluateConst p (ParamIdF _ i) =
  case find ((== i) . paramIdent_) p of
    Just param -> cata (evaluateConst p) (paramValue_ param)
    Nothing -> 0
evaluateConst _ (ConstConcatF _ c) = fold c
evaluateConst _ (ConstUnOpF _ unop c) = applyUnary unop c
evaluateConst _ (ConstBinOpF _ a binop b) = applyBinary binop a b
evaluateConst _ (ConstCondF _ a b c) = if a > 0 then b else c
evaluateConst _ (ConstStrF _ _) = 0

-- | This probably could be implemented using some recursion scheme in the
-- future. It would also be fixed by having a polymorphic expression type.
resize :: Annotation ann => Int -> ConstExpr ann -> ConstExpr ann
resize n = transformBi @_ @BitVec (\(BitVec _ a) -> BitVec n a)
