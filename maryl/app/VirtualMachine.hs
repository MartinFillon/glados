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
import GHC.IO.Handle (hIsTerminalDevice)
import Printer (Color, getColorsFromConf, reset)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (stdin)
import Utils (handleParseError, pError)
import VirtualMachine.Instructions (Instruction (..), Value (..), jump)
import VirtualMachine.Interpreter (exec)
import VirtualMachine.Operators (operators)
import VirtualMachine.Parser (parseAssembly)
import VirtualMachine.State (V (..), initialMemory, initialState)

withExit' :: Bool -> Maybe Color -> IO Value -> IO Value
withExit' True (Just e') f =
    f
        `catch` ( \e ->
                    pError
                        (show e' <> show (e :: IOException) <> reset)
                        >> return (N 84)
                )
withExit' _ _ f = f `catch` (\e -> pError (show (e :: IOException)) >> return (N 84))

withExit :: IO Value -> IO Value
withExit f =
    getColorsFromConf
        >>= (\c -> return ((\(_, e, _) -> e) <$> c))
        >>= (\c -> hIsTerminalDevice stdin >>= (\s -> withExit' s c f))

exit :: Value -> IO ()
exit (N 0) = exitSuccess
exit (N n) = exitWith $ ExitFailure (fromIntegral n)
exit e = pError ("Bad exit code " ++ show e)

execParsed :: [Instruction] -> Map String V -> IO ()
execParsed i m =
    withExit
        ( evalStateT
            exec
            (initialState (jump Nothing (Right ".start") : i) (initialMemory m) [])
        )
        >>= exit

parseOneFile :: FilePath -> IO [Either Instruction (String, [Instruction])]
parseOneFile s =
    hIsTerminalDevice stdin
        >>= (\b -> readFile s >>= (handleParseError b . parseAssembly))

vm :: [String] -> IO ()
vm [] = pError "A file is required for the vm to run"
vm files =
    foldM (\a s -> (a ++) <$> parseOneFile s) [] files
        >>= uncurry execParsed . prepareParsed

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
