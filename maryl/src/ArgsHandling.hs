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
) where

import Data.Text (pack, splitOn)
import Options.Applicative (
    Parser,
    ParserResult (Success),
    bashCompleter,
    completer,
    help,
    long,
    metavar,
    optional,
    strArgument,
    strOption,
 )
import Printer (Color, parseColor')
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

data Options = Options
    { filepath :: Maybe FilePath,
      setupColors :: Maybe String
    }
    deriving (Show)

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optional
            ( strArgument
                (metavar "FILEPATH" <> help "Optional LISP file to execute")
            )
        <*> optional
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

parsePart :: [String] -> String -> Maybe Color
parsePart [] _ = Nothing
parsePart (x : xs) part = case splitOn ":" (pack (dropWhile (== ' ') x)) of
    [p, rgb] -> if p == pack part then parseColor' rgb else parsePart xs part
    _ -> parsePart xs part

getParsedColors :: String -> Maybe (Color, Color, Color)
getParsedColors colorsStr = do
    warnings <- parsePart parts "warnings"
    errors <- parsePart parts "errors"
    infos <- parsePart parts "infos"
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
