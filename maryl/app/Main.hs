{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Main
-}

module Main (main) where

import ArgsHandling (
    Options (..),
    parseOptions,
    parseSetupColors,
 )
import Glados (glados)
import Options.Applicative (
    ParserResult (Failure, Success),
    defaultPrefs,
    execParserPure,
    fullDesc,
    handleParseResult,
    helper,
    info,
    renderFailure,
    (<**>),
 )
import Printer (setColors')
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr)

handleColorsSetup :: Options -> IO ()
handleColorsSetup opts = parseSetupColors (setupColors opts) >>= setColors'

handleArgs :: [String] -> IO ()
handleArgs args = case execParserPure
    defaultPrefs
    (info (parseOptions <**> helper) fullDesc)
    args of
    Success opts -> handleColorsSetup opts >> glados (mode opts)
    Failure res -> hPutStrLn stderr display >> exitWith exit
      where
        (display, exitCode) = renderFailure res ""
        exit =
            if exitCode == ExitSuccess
                then exitCode
                else ExitFailure 84
    res -> handleParseResult res >> mempty

main :: IO ()
main = getArgs >>= handleArgs
