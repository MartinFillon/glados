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
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Utils (handleParseError, pError)
import VirtualMachine.Instructions (Instruction (..), Value (..), jump)
import VirtualMachine.Interpreter (exec, operators)
import VirtualMachine.Parser (parseAssembly)
import VirtualMachine.State (initialState)

withExit :: IO Value -> IO Value
withExit f = f `catch` (\e -> pError (show (e :: IOException)) >> return (N 84))

exit :: Value -> IO ()
exit (N 0) = exitSuccess
exit (N n) = exitWith $ ExitFailure (fromIntegral n)
exit e = pError ("Bad exit code " ++ show e)

execParsed :: [Instruction] -> IO ()
execParsed i =
    withExit
        ( evalStateT
            exec
            (initialState (jump Nothing (Right ".start") : i) (Map.fromList operators) [])
        )
        >>= exit

vm :: Maybe String -> IO ()
vm Nothing = pError "A file is required for the vm to run"
vm (Just s) = readFile s >>= (handleParseError True . parseAssembly) >>= execParsed
