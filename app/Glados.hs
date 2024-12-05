{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}

module Glados (glados) where

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
-- add debug here for mem
handleEvalResult (Left err) = putStrLn ("Error during evaluation: " ++ err)

parseToSexpr :: Memory -> String -> IO ()
parseToSexpr mem s =
    handleParseError True (parseSexpr s)
        >>= printAndReturn
        >>= ( \sexpr -> case sexprToAST sexpr of
                Just ast -> handleEvalResult (evalAST mem ast)
                Nothing -> putStrLn "AST Conversion Error: Invalid SExpr"
            )

-- add memory

handleInput :: Memory -> String -> IO ()
handleInput = parseToSexpr

getContentFromFile :: Memory -> String -> IO ()
getContentFromFile mem filepath =
    readFile filepath
        >>= parseToSexpr mem

checkBuf' :: String -> IO String
checkBuf' s | countParenthesis s = handleInput initMemory s >> return ""
checkBuf' s = return s

checkBuf :: String -> String -> IO String
checkBuf s i = checkBuf' (s ++ ' ' : i)

getLineFromStdin' :: String -> Bool -> Bool -> IO ()
getLineFromStdin' _ _ True = return ()
getLineFromStdin' s b False = getLine >>= checkBuf s >>= (`getLineFromStdin` b)

-- Prints prompt if it's a TTY
getLineFromStdin :: String -> Bool -> IO ()
getLineFromStdin s True =
    putStr "> "
        >> flushAll
        >> isEOF
        >>= getLineFromStdin' s True
getLineFromStdin s False =
    isEOF >>= getLineFromStdin' s False

getContentFromStdin :: IO ()
getContentFromStdin = hIsTerminalDevice stdin >>= getLineFromStdin ""

glados :: Maybe String -> IO ()
glados (Just filepath) = getContentFromFile initMemory filepath
glados Nothing = getContentFromStdin
