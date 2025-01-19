{-
-- EPITECH PROJECT, 2024
-- ArgsHandling
-- File description:
-- ArgsHandling
-}
{-# LANGUAGE OverloadedStrings #-}

module ArgsHandling (
    parseOptions,
    parseSetupColors,
    handleOptionsReturn,
    Options (..),
    Mode (..),
) where

import Data.Text (pack, splitOn)
import Options.Applicative (
    Alternative (many),
    Parser,
    ParserResult (Success),
    bashCompleter,
    command,
    completer,
    help,
    hsubparser,
    info,
    long,
    metavar,
    optional,
    progDesc,
    short,
    strArgument,
    strOption,
 )
import Printer (Color, parseColor')
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

data Mode = Vm String [String] | Compile (Maybe String) (Maybe FilePath) deriving (Show)

data Options = Options
    { setupColors :: Maybe String,
      mode :: Mode
    }
    deriving (Show)

parseMode :: Parser Mode
parseMode =
    hsubparser
        ( command "build" (info parseCompile (progDesc "Build maryl asm from maryl file."))
            <> command "run" (info parseVm (progDesc "Execute the maryl asm built."))
        )

parseCompile :: Parser Mode
parseCompile =
    Compile
        <$> optional
            ( strArgument
                (metavar "File")
            )
        <*> optional
            ( strOption
                ( long "option"
                    <> short 'o'
                    <> metavar "FILEPATH"
                )
            )

parseVm :: Parser Mode
parseVm = Vm <$> strArgument (metavar "File") <*> many (strArgument (metavar "Args..."))

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optional
            ( strOption
                ( long "setup-colors"
                    <> metavar "\"warnings:R;G;B errors:R;G;B infos:R;G;B\""
                    <> help
                        ( "Optional flag to setup warnings, errors and infos colors "
                            ++ "(e.g. \"warnings:255;0;255 errors:255;0;0 infos:0;0;255\")"
                        )
                    <> completer (bashCompleter "file")
                )
            )
        <*> parseMode

parsePart :: [String] -> String -> Maybe Color
parsePart [] _ = Nothing
parsePart (x : xs) part = case splitOn ":" (pack (dropWhile (== ' ') x)) of
    [p, rgb] -> if p == pack part then parseColor' rgb else parsePart xs part
    _ -> parsePart xs part

getParsedColors :: String -> Maybe (Color, Color, Color)
getParsedColors colorsStr =
    parsePart parts "warnings" >>= \warnings ->
        parsePart parts "errors" >>= \errors ->
            parsePart parts "infos" >>= \infos ->
                Just (warnings, errors, infos)
  where
    parts = words colorsStr

parseSetupColors :: Maybe String -> IO (Maybe (Color, Color, Color))
parseSetupColors Nothing = return Nothing
parseSetupColors (Just colorsStr) = case getParsedColors colorsStr of
    Nothing ->
        hPutStrLn stderr "Invalid colors setup formatting"
            >> exitWith (ExitFailure 84)
    colors -> return colors

handleOptionsReturn :: ParserResult a -> IO ()
handleOptionsReturn (Success _) = mempty
handleOptionsReturn _ = exitWith (ExitFailure 84)
