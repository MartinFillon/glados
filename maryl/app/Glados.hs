{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}

module Glados (glados) where

import ArgsHandling (Mode (..))
import qualified Control.Monad as Monad
import Eval.Evaluator (evalAST)
import GHC.GHCi.Helpers (flushAll)
import Memory (Memory, initMemory)
import Parsing.ParserAst (parseAst, Ast(..))
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, isEOF, stdin)
import Utils (handleParseError, pError)
import VirtualMachine (vm)
import Debug.Trace (trace)

handleEvalResult :: Either String (Ast, Memory) -> IO ()
handleEvalResult (Right (result, _))
    | show result == "Void" = return ()
    | otherwise = print result
handleEvalResult (Left err) =
    pError ("*** ERROR : " ++ err)

parseSourceCode :: Memory -> String -> IO Memory
parseSourceCode mem s =
    case parseAst s of
        Left err -> trace (show s) $ do
            pError $ "*** ERROR : Invalid AST: " ++ show err
            exitWith (ExitFailure 84)
        Right asts -> trace (show asts) $ do
            -- evaluate the parsed AST
            let evalResult = evalAST mem asts
            handleEvalResult evalResult
            -- return updated memory or keep the original on error
            return $ either (const mem) snd evalResult

normalizeTabs :: String -> String
normalizeTabs [] = []
normalizeTabs (' ':xs) = detectSpaces 1 xs
normalizeTabs ('\t':xs) = '\t' : normalizeTabs xs
normalizeTabs (x:xs) = x : normalizeTabs xs

detectSpaces :: Int -> String -> String
detectSpaces count [] = replicate count ' '
detectSpaces count (' ':xs)
    | count == 3 = '\t' : normalizeTabs xs
    | otherwise = detectSpaces (count + 1) xs
detectSpaces count (x:xs) =
    replicate count ' ' ++ x : normalizeTabs xs

handleInput :: Memory -> String -> IO Memory
handleInput m s = trace (show s) $
    parseSourceCode m (normalizeTabs s)

getContentFromFile :: Memory -> String -> IO Memory
getContentFromFile mem filepath = do
    content <- readFile filepath
    handleInput mem content

checkBuf' :: Memory -> String -> IO (String, Memory)
checkBuf' mem s = handleInput mem s >>= \newMem -> return ("", newMem)

checkBuf :: Memory -> String -> String -> IO (String, Memory)
checkBuf mem s i = checkBuf' mem (if null s then i else s ++ ' ' : i ++ "\n")

getLineFromStdin' :: Memory -> String -> Bool -> Bool -> IO ()
getLineFromStdin' _ _ _ True = return ()
getLineFromStdin' mem s b False = trace s $ do
    line <- getLine
    let lineWithNewline = line ++ "\n"
    (newBuf, newMem) <- checkBuf mem s lineWithNewline
    getLineFromStdin newMem newBuf b

getLineFromStdin :: Memory -> String -> Bool -> IO ()
getLineFromStdin mem s True =
    putStr "> "
        >> flushAll
        >> isEOF
        >>= getLineFromStdin' mem s True
getLineFromStdin mem s False =
    isEOF >>= getLineFromStdin' mem s False

getContentFromStdin :: Memory -> IO ()
getContentFromStdin mem =
    hIsTerminalDevice stdin
        >>= getLineFromStdin mem ""

glados :: Mode -> Maybe String -> IO ()
glados Compile (Just filepath) = Monad.void (getContentFromFile initMemory filepath)
glados Compile Nothing = getContentFromStdin initMemory
glados Vm x = vm x
