{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}

module Glados (glados) where

import GHC.GHCi.Helpers (flushAll)
import Parsing.ErrorBundlePretty (errorBundlePrettyFormatted)
import Parsing.ParserSExpr (ParserError, parseSexpr)
import Parsing.SExprToAst (evalAST, sexprToAST)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, isEOF, stdin)

handleParseError :: Bool -> Either ParserError a -> IO a
handleParseError _ (Right val) = return val
handleParseError showColors (Left err) =
    putStr (errorBundlePrettyFormatted showColors err)
        >> exitWith (ExitFailure 1)
        >> return undefined

parseToSexpr :: String -> IO ()
parseToSexpr s = handleParseError True (parseSexpr s) >>= (\x -> print (sexprToAST x >>= evalAST))

handleInput :: String -> IO ()
handleInput = parseToSexpr

handleContentFromFile :: [String] -> IO ()
handleContentFromFile = foldr ((>>) . handleInput) (return ())

getContentFromFile :: String -> IO ()
getContentFromFile filepath =
    readFile filepath
        >>= handleContentFromFile . lines

getLineFromStdin' :: Bool -> Bool -> IO ()
getLineFromStdin' _ True = return ()
getLineFromStdin' b False = getLine >>= handleInput >> getLineFromStdin b

-- Prints prompt if it's a TTY
getLineFromStdin :: Bool -> IO ()
getLineFromStdin True =
    putStr "> "
        >> flushAll
        >> isEOF
        >>= getLineFromStdin' True
getLineFromStdin False =
    isEOF >>= getLineFromStdin' False

getContentFromStdin :: IO ()
getContentFromStdin = hIsTerminalDevice stdin >>= getLineFromStdin

glados :: Maybe String -> IO ()
glados (Just filepath) = getContentFromFile filepath
glados Nothing = getContentFromStdin
