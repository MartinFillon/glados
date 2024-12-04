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

handleEvalResult :: Either String Ast -> IO ()
handleEvalResult (Right result) = print result
handleEvalResult (Left err) = putStrLn ("Error during evaluation: " ++ err)

parseToSexpr :: String -> IO ()
parseToSexpr s =
    handleParseError True (parseSexpr s)
        >>= printAndReturn
        >>= (\sexpr -> case sexprToAST sexpr of
            Nothing -> putStrLn "AST Conversion Error: Invalid SExpr"
            Just ast -> handleEvalResult (evalAST ast)
            )

-- add memory

handleInput :: String -> IO ()
handleInput = parseToSexpr

getContentFromFile :: String -> IO ()
getContentFromFile filepath =
    readFile filepath
        >>= parseToSexpr

checkBuf' :: String -> IO String
checkBuf' s | countParenthesis s = handleInput s >> return ""
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
glados (Just filepath) = getContentFromFile filepath
glados Nothing = getContentFromStdin
