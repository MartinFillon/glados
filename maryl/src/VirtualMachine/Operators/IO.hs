{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- IO
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}

module VirtualMachine.Operators.IO (
    operatorPrint,
    operatorReadFile,
    operatorWriteFile,
    operatorAppendFile,
    opOpenFile,
    opCloseHandle,
) where

import Data.Functor ((<&>))
import Data.Int (Int64)
import GHC.IO.Handle (Handle)
import System.IO (IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode), hClose, openFile)
import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState, getHandleInMemory, io, ioCatch, registerHandle)

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

opOpenFile' :: String -> IOMode -> VmState (Either Handle Int64)
opOpenFile' n m = ioCatch (openFile n m) (-1)

opOpenFile'' :: Either Handle Int64 -> VmState Value
opOpenFile'' (Right n) = pure $ N n
opOpenFile'' (Left h) = N <$> registerHandle h

opOpenFile :: [Value] -> VmState [Value]
opOpenFile (S name : S "r" : xs) = (opOpenFile' name ReadMode >>= opOpenFile'') <&> (: xs)
opOpenFile (S name : S "w" : xs) = (opOpenFile' name WriteMode >>= opOpenFile'') <&> (: xs)
opOpenFile (S name : S "rw" : xs) = (opOpenFile' name ReadWriteMode >>= opOpenFile'') <&> (: xs)
opOpenFile (S name : S "a" : xs) = (opOpenFile' name AppendMode >>= opOpenFile'') <&> (: xs)
opOpenFile xs = pure $ N (-1) : xs

opCloseHandle' :: Either () Int64 -> VmState Value
opCloseHandle' (Right n) = pure $ N n
opCloseHandle' (Left _) = pure $ N 0

opCloseHandle :: [Value] -> VmState [Value]
opCloseHandle (N hdl : xs) = (getHandleInMemory hdl >>= (\h -> ioCatch (hClose h) (-1)) >>= opCloseHandle') <&> (: xs)
opCloseHandle xs = pure (N (-1) : xs)
