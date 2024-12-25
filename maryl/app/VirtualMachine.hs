{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- VirtualMachine
-}

module VirtualMachine (vm) where

import Control.Monad.State
import Utils (handleParseError, pError)
import VirtualMachine.Instructions (Instruction (..))
import VirtualMachine.Interpreter (exec)
import VirtualMachine.Parser (parseAssembly)
import VirtualMachine.State (initialState)

execParsed :: [Instruction] -> IO ()
execParsed i = evalStateT exec (initialState i []) >>= print

vm :: Maybe String -> IO ()
vm Nothing = pError "A file is required for the vm to run"
vm (Just s) = readFile s >>= (handleParseError True . parseAssembly) >>= execParsed
