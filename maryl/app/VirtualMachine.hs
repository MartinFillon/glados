{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- VirtualMachine
-}

module VirtualMachine (vm) where

import Control.Exception (IOException, catch)
import Control.Monad.State (evalStateT, foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Utils (handleParseError, pError)
import VirtualMachine.Instructions (Instruction (..), Value (..), jump)
import VirtualMachine.Interpreter (exec, operators)
import VirtualMachine.Parser (parseAssembly)
import VirtualMachine.State (V (..), initialState)

withExit :: IO Value -> IO Value
withExit f = f `catch` (\e -> pError (show (e :: IOException)) >> return (N 84))

exit :: Value -> IO ()
exit (N 0) = exitSuccess
exit (N n) = exitWith $ ExitFailure (fromIntegral n)
exit e = pError ("Bad exit code " ++ show e)

execParsed :: [Instruction] -> Map String V -> IO ()
execParsed i m =
    withExit
        ( evalStateT
            exec
            (initialState (jump Nothing (Right ".start") : i) m [])
        )
        >>= exit

parseOneFile :: FilePath -> IO [Instruction]
parseOneFile s = readFile s >>= (handleParseError True . parseAssembly)

vm :: [String] -> IO ()
vm [] = pError "A file is required for the vm to run"
vm files =
    foldM (\a s -> (a ++) <$> parseOneFile s) [] files
        >>= execParsed

prepareParsed'' :: [Either Instruction (String, [Instruction])] -> Map String V
prepareParsed'' [] = Map.fromList operators
prepareParsed'' (Right (n, is) : xs) = Map.insert n (V $ Bi is) (prepareParsed'' xs)
prepareParsed'' (_ : xs) = prepareParsed'' xs

prepareParsed' :: [Either Instruction (String, [Instruction])] -> [Instruction]
prepareParsed' [] = []
prepareParsed' (Left i : xs) = i : prepareParsed' xs
prepareParsed' (_ : xs) = prepareParsed' xs

prepareParsed ::
    [Either Instruction (String, [Instruction])] -> ([Instruction], Map String V)
prepareParsed l = (prepareParsed' l, prepareParsed'' l)

vm :: [String] -> IO ()
vm [] = pError "A file is required for the vm to run"
vm files =
    (readFile s >>= (handleParseError True . parseAssembly))
        >>= uncurry execParsed . prepareParsed
