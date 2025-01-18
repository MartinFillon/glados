{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Binary
-}

module VirtualMachine.Operators.Binary (binaryAnd, binaryOr, binaryXor, binaryShiftR, binaryShiftL) where

import Data.Bits (Bits (shiftL, shiftR, xor, (.|.)), (.&.))
import Data.Int (Int64)
import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState, eitherS)

binaryOp ::
    (Int64 -> Int64 -> Int64) -> Value -> Value -> Either String Value
binaryOp op (N x) (N y) = Right $ N (op x y)
binaryOp _ _ _ = Left "Invalid binary op"

binaryAnd :: [Value] -> VmState [Value]
binaryAnd (y : x : xs) = eitherS $ (: xs) <$> binaryOp (.&.) x y
binaryAnd _ = fail "expects two number"

binaryOr :: [Value] -> VmState [Value]
binaryOr (y : x : xs) = eitherS $ (: xs) <$> binaryOp (.|.) x y
binaryOr _ = fail "expects two number"

binaryXor :: [Value] -> VmState [Value]
binaryXor (y : x : xs) = eitherS $ (: xs) <$> binaryOp xor x y
binaryXor _ = fail "expects two number"

binaryShiftR :: [Value] -> VmState [Value]
binaryShiftR (N n : N v : xs) = pure $ N (shiftR v (fromIntegral n)) : xs
binaryShiftR _ = fail "expects two ints"

binaryShiftL :: [Value] -> VmState [Value]
binaryShiftL (N n : N v : xs) = pure $ N (shiftL v (fromIntegral n)) : xs
binaryShiftL _ = fail "expects two ints"
