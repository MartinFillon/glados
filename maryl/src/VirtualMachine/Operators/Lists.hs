{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Lists
-}

module VirtualMachine.Operators.Lists (
    operatorGet,
    operatorSet,
    listPop,
    listPush,
) where

import Data.Int (Int64)
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

remove :: Int64 -> [a] -> [a]
remove _ [] = []
remove 0 (_ : xs) = xs
remove n (x : xs) = x : remove (n - 1) xs

listPop :: [Value] -> VmState [Value]
listPop (N idx : L lst : xs)
    | idx >= 0 && idx < fromIntegral (length lst) =
        return $ L (remove idx lst) : xs
    | otherwise = fail "Index out of bound"
listPop (N idx : S str : xs)
    | idx >= 0 && idx < fromIntegral (length str) =
        return $ S (remove idx str) : xs
    | otherwise = fail "Index out of bound"
listPop _ = fail "expects a list and an integer index"

listPush :: [Value] -> VmState [Value]
listPush (val : L lst : xs) = pure $ L (lst ++ [val]) : xs
listPush ((C ch) : S str : xs) = pure $ S (str ++ [ch]) : xs
listPush _ = fail "expects a list, an integer index, and a value"
