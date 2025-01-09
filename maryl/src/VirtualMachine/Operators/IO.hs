{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- IO
-}

module VirtualMachine.Operators.IO (
    operatorPrint,
    operatorReadFile,
    operatorWriteFile,
    operatorAppendFile,
) where

import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState, io)

operatorPrint :: [Value] -> VmState [Value]
operatorPrint (S s : xs) = io $ putStr s >> return (N (fromIntegral (length s)) : xs)
operatorPrint (C c : xs) = io $ putChar c >> return (N 1 : xs)
operatorPrint (val : xs) =
    io $ (putStr . show) val >> return (N (fromIntegral (length (show val))) : xs)
operatorPrint _ = fail "expects one val"

operatorReadFile :: [Value] -> VmState [Value]
operatorReadFile (S path : xs) = io $ do
    content <- readFile path
    return (S content : xs)
operatorReadFile _ = fail "expects a string path"

operatorWriteFile :: [Value] -> VmState [Value]
operatorWriteFile (S content : S path : xs) = io $ do
    writeFile path content
    return (N (fromIntegral $ length content) : xs)
operatorWriteFile _ = fail "expects a string path and string content"

operatorAppendFile :: [Value] -> VmState [Value]
operatorAppendFile (S content : S path : xs) = io $ do
    appendFile path content
    return (N (fromIntegral $ length content) : xs)
operatorAppendFile _ = fail "expects a string path and string content"
