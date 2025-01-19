{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- Utils
-}

module Utils (handleParseError, pError) where

import Data.Text (pack)
import Data.Text.IO (hPutStrLn)
import Data.Void (Void)
import ErrorBundlePretty (errorBundlePrettyFormatted)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)
import Text.Megaparsec.Error (ParseErrorBundle)

type ParserError = ParseErrorBundle String Void

pError :: String -> IO ()
pError s = hPutStrLn stderr (pack s) >> exitWith (ExitFailure 84)

handleParseError :: Bool -> Either ParserError a -> IO a
handleParseError _ (Right val) = return val
handleParseError showColors (Left err) =
    errorBundlePrettyFormatted showColors err
        >>= pError
        >> exitWith (ExitFailure 84)
        >> return undefined
