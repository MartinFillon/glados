{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Printer (
    Paint (..),
    Style (..),
    Color (..),
    ColorCode (..),
    Config (..),
    reset,
    setColors,
    setColors',
    getColorsFromConf,
    getColorCode,
    parseColor,
    parseColor',
    parseRGB,
    parseConf,
    confDefaultValues,
    confFilepath,
    parseColors,
    ParserError,
) where

import Data.Either (rights)
import Data.List (find)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Void (Void)
import Data.Word (Word8)

import Control.Monad (void)
import System.Directory (doesFileExist)
import Text.Megaparsec (
    ParseErrorBundle,
    Parsec,
    empty,
    many,
    manyTill,
    runParser,
    some,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

confFilepath :: String
confFilepath = "maryl-colors.conf"

confDefaultValues :: String
confDefaultValues =
    "warnings=\"255;0;255\"\n"
        ++ "errors=\"255;0;0\"\n"
        ++ "infos=\"0;0;255\""

data Config = Config
    { key :: String,
      value :: String
    }
    deriving (Show, Eq)

newtype ColorCode = ColorCode (Word8, Word8, Word8) deriving (Eq)

instance Show ColorCode where
    show :: ColorCode -> String
    show (ColorCode (r, g, b)) = show r ++ ";" ++ show g ++ ";" ++ show b

data Color
    = Red
    | Green
    | Blue
    | Yellow
    | Magenta
    | Cyan
    | White
    | Orange
    | RGB ColorCode
    deriving (Eq)

instance Show Color where
    show :: Color -> String
    show Red = "\x1b[31m"
    show Green = "\x1b[32m"
    show Blue = "\x1b[34m"
    show Yellow = "\x1b[33m"
    show Magenta = "\x1b[35m"
    show Cyan = "\x1b[36m"
    show White = "\x1b[37m"
    show Orange = "\x1b[38;2;255;128;0m"
    show (RGB c) = "\x1b[38;2;" ++ show c ++ "m"

reset :: String
reset = "\x1b[0m"

data Style
    = Bold
    | Underlined

instance Show Style where
    show :: Style -> String
    show Bold = "\x1b[1m"
    show Underlined = "\x1b[4m"

class Paint a where
    paint :: Bool -> a -> String

getColorCode :: Color -> ColorCode
getColorCode Red = ColorCode (255, 0, 0)
getColorCode Green = ColorCode (0, 255, 0)
getColorCode Blue = ColorCode (0, 0, 255)
getColorCode Yellow = ColorCode (255, 255, 0)
getColorCode Magenta = ColorCode (255, 0, 255)
getColorCode Cyan = ColorCode (0, 255, 255)
getColorCode White = ColorCode (255, 255, 255)
getColorCode Orange = ColorCode (255, 128, 0)
getColorCode (RGB c) = c

setColors :: Color -> Color -> Color -> IO ()
setColors w e i =
    writeFile
        confFilepath
        ( "warnings=\""
            ++ show (getColorCode w)
            ++ "\"\nerrors=\""
            ++ show (getColorCode e)
            ++ "\"\ninfos=\""
            ++ show (getColorCode i)
            ++ "\""
        )

setColors' :: Maybe (Color, Color, Color) -> IO ()
setColors' (Just (w, e, i)) = setColors w e i
setColors' Nothing = mempty

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

variable :: Parser String
variable = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

keyParser :: Parser String
keyParser = variable

valueParser :: Parser String
valueParser = stringLiteral

keyValueParser :: Parser Config
keyValueParser = do
    k <- lexeme keyParser
    _ <- lexeme $ char '='
    Config k <$> lexeme valueParser

parseConf :: Text -> [Either ParserError Config]
parseConf t = runParser keyValueParser "" <$> splitOn (pack "\n") t

parseColor :: String -> Maybe Color
parseColor color = case unpack <$> splitOn (pack ";") (pack color) of
    [r, g, b] -> Just $ parseRGB r g b
    _ -> Nothing

parseColor' :: Text -> Maybe Color
parseColor' color = case unpack <$> splitOn (pack ";") color of
    [r, g, b] -> Just $ parseRGB r g b
    _ -> Nothing

parseRGB :: String -> String -> String -> Color
parseRGB r g b = RGB $ ColorCode (read r :: Word8, read g :: Word8, read b :: Word8)

searchValue :: String -> [Config] -> Maybe String
searchValue k = fmap value . find ((== k) . key)

parseColors :: [Config] -> Maybe (Color, Color, Color)
parseColors configs = do
    warnings <- searchValue "warnings" configs
    errors <- searchValue "errors" configs
    infos <- searchValue "infos" configs
    (,,) <$> parseColor warnings <*> parseColor errors <*> parseColor infos

getColorsFromConf :: IO (Maybe (Color, Color, Color))
getColorsFromConf =
    doesFileExist confFilepath
        >>= \case
            True -> do
                content <- readFile confFilepath
                return $ parseColors (rights $ parseConf (pack content))
            False -> do
                writeFile confFilepath confDefaultValues
                return $ parseColors (rights $ parseConf (pack confDefaultValues))
