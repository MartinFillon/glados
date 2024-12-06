{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}

module Glados (glados) where

import qualified Control.Monad as Monad
import ErrorBundlePretty (errorBundlePrettyFormatted)
import Eval.Evaluator (evalAST)
import GHC.GHCi.Helpers (flushAll)
import Memory (Memory, initMemory)
import Parsing.ParserSExpr (ParserError, parseSexpr)
import Parsing.SExprToAst (Ast (..), sexprToAST)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, isEOF, stdin)

countChar :: Char -> String -> Int
countChar c s = length (filter (== c) s)

countParenthesis :: String -> Bool
countParenthesis s = countChar '(' s == countChar ')' s

handleParseError :: Bool -> Either ParserError a -> IO a
handleParseError _ (Right val) = return val
handleParseError showColors (Left err) =
    errorBundlePrettyFormatted showColors err
        >>= putStr
        >> exitWith (ExitFailure 84)
        >> return undefined

printAndReturn :: Show a => a -> IO a
printAndReturn x = print x >> return x

handleEvalResult :: Either String (Ast, Memory) -> IO ()
handleEvalResult (Right (result, _)) = print result
-- handleEvalResult (Right (result, mem)) = print result >> putStrLn "mem state: " >> print mem
handleEvalResult (Left err) = putStrLn ("Error during evaluation: " ++ err)

parseToSexpr :: Memory -> String -> IO Memory
parseToSexpr mem s =
    handleParseError True (parseSexpr s)
        >>= printAndReturn
        >>= maybe
            (putStrLn "AST Conversion Error: Invalid SExpr" >> return mem)
            ( \ast ->
                handleEvalResult (evalAST mem ast)
                    >> return (either (const mem) snd (evalAST mem ast))
            )
            . sexprToAST

handleInput :: Memory -> String -> IO Memory
handleInput = parseToSexpr

getContentFromFile :: Memory -> String -> IO Memory
getContentFromFile mem filepath = readFile filepath >>= parseToSexpr mem

checkBuf' :: Memory -> String -> IO (String, Memory)
checkBuf' mem s
    | countParenthesis s = handleInput mem s >>= \newMem -> return ("", newMem)
    | otherwise = return (s, mem)

checkBuf :: Memory -> String -> String -> IO (String, Memory)
checkBuf mem s i = checkBuf' mem (s ++ ' ' : i)

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

glados :: Maybe String -> IO ()
glados (Just filepath) = Monad.void (getContentFromFile initMemory filepath)
glados Nothing = getContentFromStdin initMemory
