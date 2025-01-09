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
operatorEq _ = fail "expects two value"

operatorNEq :: [Value] -> VmState [Value]
operatorNEq (x : y : xs) = return $ B (x /= y) : xs
operatorNEq _ = fail "expects two value"

operatorLt :: [Value] -> VmState [Value]
operatorLt (y : x : xs) =
    eitherS $
        (: xs)
            <$> case (x, y) of
                (N a, N b) -> Right $ B (toDouble a < toDouble b)
                (N a, D b) -> Right $ B (toDouble a < b)
                (D a, N b) -> Right $ B (a < toDouble b)
                (D a, D b) -> Right $ B (a < b)
                _ -> Left "expects two number"
operatorLt _ = fail "expects two number"

operatorGt :: [Value] -> VmState [Value]
operatorGt (y : x : xs) =
    eitherS $
        (: xs)
            <$> case (x, y) of
                (N a, N b) -> Right $ B (toDouble a > toDouble b)
                (N a, D b) -> Right $ B (toDouble a > b)
                (D a, N b) -> Right $ B (a > toDouble b)
                (D a, D b) -> Right $ B (a > b)
                _ -> Left "expects two number"
operatorGt _ = fail "expects two number"

operatorAnd :: [Value] -> VmState [Value]
operatorAnd (B y : B x : xs) = return $ B (x && y) : xs
operatorAnd _ = fail "And expects two bool"

operatorOr :: [Value] -> VmState [Value]
operatorOr (B y : B x : xs) = return $ B (x || y) : xs
operatorOr _ = fail "Or expects two booleans"

logicalNot :: [Value] -> VmState [Value]
logicalNot (B y : xs) = return $ B (not y) : xs
logicalNot _ = fail "Or expects two booleans"
