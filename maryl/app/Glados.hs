{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}

module Glados (glados) where

import ArgsHandling (Mode (..))
import Compiler.ASTtoASM (translateToASM)
import Compiler.WriteASM (writeInstructionsToFile)
import qualified Control.Monad as Monad
import Debug.Trace (trace)
import Eval.Evaluator (evalAST)
import GHC.GHCi.Helpers (flushAll)
import Memory (Memory, initMemory)
import Parsing.ParserAst (Ast (..), parseAST)
import System.IO (hIsTerminalDevice, isEOF, stdin)
import Utils (handleParseError, pError)
import VirtualMachine (vm)

handleEvalResult :: Either String ([Ast], Memory) -> IO ()
handleEvalResult (Right (result, mem)) = do
    let instructions = translateToASM result
    writeInstructionsToFile "out.s" mem
    print result
handleEvalResult (Left err) =
    pError ("*** ERROR : " ++ err)

parseSourceCode :: Memory -> String -> IO Memory
parseSourceCode mem s = do
    asts <- handleParseError True (parseAST s)
    let evalResult = evalAST mem asts
    handleEvalResult evalResult
    return $ either (const mem) snd evalResult

normalizeTabs :: String -> String
normalizeTabs [] = []
normalizeTabs (' ' : xs) = detectSpaces 1 xs
normalizeTabs ('\t' : xs) = '\t' : normalizeTabs xs
normalizeTabs (x : xs) = x : normalizeTabs xs

detectSpaces :: Int -> String -> String
detectSpaces count [] = replicate count ' '
detectSpaces count (' ' : xs)
    | count == 3 = '\t' : normalizeTabs xs
    | otherwise = detectSpaces (count + 1) xs
detectSpaces count (x : xs) =
    replicate count ' ' ++ x : normalizeTabs xs

handleInput :: Memory -> String -> IO Memory
handleInput m s = parseSourceCode m (normalizeTabs s)

getContentFromFile :: Memory -> String -> IO Memory
getContentFromFile mem filepath =
    readFile filepath >>= handleInput mem

countChar :: Char -> String -> Int
countChar c s = length (filter (== c) s)

countBrackets :: String -> Bool
countBrackets s = countChar '{' s == countChar '}' s

checkBuf' :: Memory -> String -> IO (String, Memory)
checkBuf' mem s
    | countBrackets s = handleInput mem s >>= \newMem -> return ("", newMem)
    | otherwise = return (s, mem)

checkBuf :: Memory -> String -> String -> IO (String, Memory)
checkBuf mem s i
    | s /= "" = checkBuf' mem (s ++ '\n' : i)
    | otherwise = checkBuf' mem i

-- Prints prompt if it's a TTY
getLineFromStdin' :: Memory -> String -> Bool -> Bool -> IO ()
getLineFromStdin' _ _ _ True = return ()
getLineFromStdin' mem s b False =
    getLine
        >>= ( checkBuf mem s
                Monad.>=> (\(newBuf, newMem) -> getLineFromStdin newMem newBuf b)
            )

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
