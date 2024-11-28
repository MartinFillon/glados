{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser SExpr
-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.ParserSExpr (
    parseSexpr,
    pOperator,
    pOperator',
    pVariable,
    pDigit,
    lineComment,
    sc,
    handleParseError,
    Sexpr (..),
    Atom (..),
) where

import Control.Applicative (Alternative (many))
import Control.Monad (void)
import Data.Void (Void)
import Parsing.ErrorBundlePretty (errorBundlePrettyFormatted)
import Text.Megaparsec (
    MonadParsec (eof),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    empty,
    parse,
    some,
    try,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

data Atom = String String | Number Int | Float Float | Bool Bool deriving (Show, Eq)

data Sexpr = Atom Atom | List [Sexpr] deriving (Show, Eq)

pOperator :: Parser Atom
pOperator = String <$> pOperator'

pOperator' :: Parser String
pOperator' =
    choice
        [ "+",
          "-",
          "*",
          "<",
          "eq?",
          "if",
          "div",
          "mod"
        ]

pVariable :: Parser Atom
pVariable = String <$> pVariable'

pVariable' :: Parser String
pVariable' = (:) <$> letterChar <*> many alphaNumChar <?> "variable"

pDigit :: Parser Atom
pDigit = Number <$> L.signed (L.space empty empty empty) L.decimal

pFloat :: Parser Atom
pFloat = Float <$> L.signed (L.space empty empty empty) L.float

parseFalse :: Parser Atom
parseFalse = string "#f" >> return (Bool False)

parseTrue :: Parser Atom
parseTrue = string "#t" >> return (Bool True)

pBool :: Parser Atom
pBool = choice [parseFalse, parseTrue]

convertValue :: Parser Atom
convertValue =
    choice
        [ try pFloat,
          try pDigit,
          pOperator,
          try pBool,
          try pVariable
        ]

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseAtom :: Parser Sexpr
parseAtom = Atom <$> convertValue

parseList :: Parser Sexpr
parseList = lexeme $ List <$> between (char '(') (char ')') (many parseBasic)

parseBasic :: Parser Sexpr
parseBasic = lexeme $ try parseList <|> parseAtom

parseSexpr :: String -> Either ParserError Sexpr
parseSexpr = parse (between sc eof parseBasic) ""

handleParseError :: Show a => Bool -> Either ParserError a -> IO ()
handleParseError _ (Right val) = print val
handleParseError showColors (Left err) = putStr $ errorBundlePrettyFormatted showColors err
