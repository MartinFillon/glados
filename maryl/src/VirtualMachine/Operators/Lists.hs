{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Lists
-}

module VirtualMachine.Operators.Lists (operatorGet, operatorSet) where

import VirtualMachine.Instructions (Value (C, L, N, S))
import VirtualMachine.State (VmState)

operatorGet :: [Value] -> VmState [Value]
operatorGet (N idx : L lst : xs)
    | idx >= 0 && idx < fromIntegral (length lst) =
        return $ (lst !! fromIntegral idx) : xs
    | otherwise = fail "Index out of bound"
operatorGet (N idx : S str : xs)
    | idx >= 0 && idx < fromIntegral (length str) =
        return $ C (str !! fromIntegral idx) : xs
    | otherwise = fail "Index out of bound"
operatorGet _ = fail "expects a list and an integer index"

operatorSet :: [Value] -> VmState [Value]
operatorSet (val : N idx : L lst : xs)
    | idx >= 0 && idx < fromIntegral (length lst) =
        return $
            L (take (fromIntegral idx) lst ++ [val] ++ drop (fromIntegral idx + 1) lst)
                : xs
    | otherwise = fail "Index out of bound"
operatorSet ((C ch) : N idx : S str : xs)
    | idx >= 0 && idx < fromIntegral (length str) =
        return $
            S (take (fromIntegral idx) str ++ [ch] ++ drop (fromIntegral idx + 1) str)
                : xs
    | otherwise = fail "Index out of bound"
operatorSet _ = fail "expects a list, an integer index, and a value"
