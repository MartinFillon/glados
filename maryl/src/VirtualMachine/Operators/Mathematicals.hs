{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Mathematicals
-}

module VirtualMachine.Operators.Mathematicals (operatorAdd, operatorSub, operatorMul, operatorDiv, operatorMod, operatorPow) where

import Numeric (Numeric (..))
import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState, eitherS)

-------------------------
-- operator operations --

-------------------------

numericOp ::
    (Double -> Double -> Double) -> Value -> Value -> Either String Value
numericOp op (N x) (N y) = case fromDouble (op (toDouble x) (toDouble y)) of
    Right n -> Right $ N n
    Left _ -> Right $ D (op (toDouble x) (toDouble y))
numericOp op (N x) (D y) = Right $ D (op (toDouble x) y)
numericOp op (D x) (N y) = Right $ D (op x (toDouble y))
numericOp op (D x) (D y) = Right $ D (op x y)
numericOp _ _ _ = Left "Invalid numeric op"

operatorAdd :: [Value] -> VmState [Value]
operatorAdd (y : x : xs) = eitherS $ (: xs) <$> numericOp (+) x y
operatorAdd _ = fail "expects two number"

operatorSub :: [Value] -> VmState [Value]
operatorSub (y : x : xs) = eitherS $ (: xs) <$> numericOp (-) x y
operatorSub _ = fail "expects two number"

operatorMul :: [Value] -> VmState [Value]
operatorMul (y : x : xs) = eitherS $ (: xs) <$> numericOp (*) x y
operatorMul _ = fail "expects two number"

operatorDiv :: [Value] -> VmState [Value]
operatorDiv (y : x : xs) =
    case y of
        N y' | y' == 0 -> fail "division by zero"
        D y' | y' == 0.0 -> fail "division by zero"
        _ -> eitherS $ (: xs) <$> numericOp (/) x y
operatorDiv _ = fail "expects two number"

operatorMod :: [Value] -> VmState [Value]
operatorMod (N y : N x : xs)
    | y == 0 = fail "modulo by zero"
    | otherwise = return $ N (x `mod` y) : xs
operatorMod _ = fail "expects two int"

operatorPow :: [Value] -> VmState [Value]
operatorPow (y : x : xs) = eitherS $ (: xs) <$> numericOp (**) x y
operatorPow _ = fail "expects two number"