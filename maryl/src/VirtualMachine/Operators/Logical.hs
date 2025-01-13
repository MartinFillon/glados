{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Logical
-}

module VirtualMachine.Operators.Logical (
    operatorEq,
    operatorNEq,
    operatorLt,
    operatorGt,
    operatorAnd,
    operatorOr,
    logicalNot,
) where

import Numeric (Numeric (..))
import VirtualMachine.Instructions (Value (B, D, N))
import VirtualMachine.State (VmState, eitherS)

operatorEq :: [Value] -> VmState [Value]
operatorEq (x : y : xs) = return $ B (x == y) : xs
operatorEq _ = fail "Eq expects two value"

operatorNEq :: [Value] -> VmState [Value]
operatorNEq (x : y : xs) = return $ B (x /= y) : xs
operatorNEq _ = fail "Neq expects two value"

operatorLt :: [Value] -> VmState [Value]
operatorLt (N y : N x : xs) = pure ((: xs) (B (toDouble x < toDouble y)))
operatorLt (D y : N x : xs) = pure ((: xs) (B (toDouble x < toDouble y)))
operatorLt (N y : D x : xs) = pure ((: xs) (B (toDouble x < toDouble y)))
operatorLt (D y : D x : xs) = pure ((: xs) (B (toDouble x < toDouble y)))
operatorLt _ = fail "Lesser expects two number"

operatorGt :: [Value] -> VmState [Value]
operatorGt (N y : N x : xs) = pure ((: xs) (B (toDouble x > toDouble y)))
operatorGt (D y : N x : xs) = pure ((: xs) (B (toDouble x > toDouble y)))
operatorGt (N y : D x : xs) = pure ((: xs) (B (toDouble x > toDouble y)))
operatorGt (D y : D x : xs) = pure ((: xs) (B (toDouble x > toDouble y)))
operatorGt _ = fail "Greater expects two number"

operatorAnd :: [Value] -> VmState [Value]
operatorAnd (B y : B x : xs) = return $ B (x && y) : xs
operatorAnd _ = fail "And expects two booleans"

operatorOr :: [Value] -> VmState [Value]
operatorOr (B y : B x : xs) = return $ B (x || y) : xs
operatorOr _ = fail "Or expects two booleans"

logicalNot :: [Value] -> VmState [Value]
logicalNot (B y : xs) = return $ B (not y) : xs
logicalNot _ = fail "Not expects two booleans"
