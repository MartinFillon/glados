{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Main
-}

module Main (main) where

import Glados (glados)
import System.Environment
import ArgsHandling
import Printer (setColors')
import System.Exit
import System.IO (hPutStrLn, stderr)
import Options.Applicative

handleColorsSetup :: Options -> IO ()
handleColorsSetup opts = parseSetupColors (setupColors opts) >>= setColors'

handleArgs :: [String] -> IO ()
handleArgs args = case execParserPure defaultPrefs
            (info (parseOptions <**> helper) fullDesc) args of
        Success opts -> handleColorsSetup opts >> glados (filepath opts)
        Failure res -> hPutStrLn stderr display >> exitWith exit
            where
                (display, exitCode) = renderFailure res ""
                exit = if exitCode == ExitSuccess
                    then exitCode
                    else ExitFailure 84
        res -> handleParseResult res >> mempty

main :: IO ()
main = getArgs >>= handleArgs
