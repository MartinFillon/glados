module Main (main) where

import Glados (glados)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> glados (Just filepath)
        _ -> glados Nothing
