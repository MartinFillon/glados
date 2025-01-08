{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- IO
-}

module VirtualMachine.Operators.IO (operatorPrint) where

import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState, io)

operatorPrint :: [Value] -> VmState [Value]
operatorPrint (S s : xs) = io $ putStr s >> return (N (fromIntegral (length s)) : xs)
operatorPrint (C c : xs) = io $ putChar c >> return (N 1 : xs)
operatorPrint (val : xs) =
    io $ (putStr . show) val >> return (N (fromIntegral (length (show val))) : xs)
operatorPrint _ = fail "expects one val"
