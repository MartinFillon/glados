{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- VirtualMachine
-}

module VirtualMachine (vm) where

import Control.Exception (IOException, catch)
import Control.Monad.State (evalStateT)
import qualified Data.Map as Map
import Utils (handleParseError, pError)
import VirtualMachine.Instructions (Instruction (..), jump)
import VirtualMachine.Interpreter (exec, operators)
import VirtualMachine.Parser (parseAssembly)
import VirtualMachine.State (initialState)

withExit :: IO () -> IO ()
withExit f = f `catch` (\e -> pError (show (e :: IOException)))

execParsed :: [Instruction] -> IO ()
execParsed i =
    withExit $
        evalStateT
            exec
            (initialState (jump Nothing (Right ".start") : i) (Map.fromList operators) [])
            >>= print

vm :: Maybe String -> IO ()
vm Nothing = pError "A file is required for the vm to run"
vm (Just s) = readFile s >>= (handleParseError True . parseAssembly) >>= execParsed
