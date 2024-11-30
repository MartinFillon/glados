{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Lib
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use if" #-}

module Lib (glados) where

import GHC.GHCi.Helpers (flushAll)
import System.IO (hIsTerminalDevice, isEOF, stdin)

handleInput :: String -> IO ()
handleInput = putStrLn

handleContentFromFile :: [String] -> IO ()
handleContentFromFile = foldr ((>>) . handleInput) (return ())

getContentFromFile :: String -> IO ()
getContentFromFile filepath =
  readFile filepath
    >>= handleContentFromFile . lines

-- Prints prompt if it's a TTY
getLineFromStdin :: Bool -> IO ()
getLineFromStdin True =
  putStr "> "
    >> flushAll
    >> isEOF
    >>= \end -> case end of
      True -> return ()
      False -> getLine >>= handleInput >> getLineFromStdin True
getLineFromStdin False =
  isEOF >>= \end -> case end of
    True -> return ()
    False -> getLine >>= handleInput >> getLineFromStdin False

getContentFromStdin :: IO ()
getContentFromStdin = hIsTerminalDevice stdin >>= getLineFromStdin

glados :: Maybe String -> IO ()
glados (Just filepath) = getContentFromFile filepath
glados Nothing = getContentFromStdin
