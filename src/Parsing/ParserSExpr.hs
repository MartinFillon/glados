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
    Sexpr (..),
    Atom (..),
    ParserError,
) where

import Control.Applicative (Alternative (many))
import Control.Monad (void)
import Data.Void (Void)
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

data Atom i f
    = String String
    | Number i
    | Float f
    | Bool Bool
    deriving (Show, Eq)

data Sexpr i f = Atom (Atom i f) | List [Sexpr i f] deriving (Show, Eq)

pOperator :: Parser (Atom i f)
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

pVariable :: Parser (Atom i f)
pVariable = String <$> pVariable'

pVariable' :: Parser String
pVariable' = (:) <$> letterChar <*> many alphaNumChar <?> "variable"

sce :: Parser ()
sce = L.space empty empty empty

pDigit :: (Num i) => Parser (Atom i f)
pDigit = Number <$> L.signed sce L.decimal

pFloat :: (RealFloat f) => Parser (Atom i f)
pFloat = Float <$> L.signed sce L.float

parseFalse :: Parser (Atom i f)
parseFalse = string "#f" >> return (Bool False)

parseTrue :: Parser (Atom i f)
parseTrue = string "#t" >> return (Bool True)

pBool :: Parser (Atom i f)
pBool = choice [parseFalse, parseTrue]

convertValue :: (Num i, RealFloat f) => Parser (Atom i f)
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

parseAtom :: (Num i, RealFloat f) => Parser (Sexpr i f)
parseAtom = Atom <$> convertValue

parseList :: (Num i, RealFloat f) => Parser (Sexpr i f)
parseList = lexeme $ List <$> between (char '(') (char ')') (many parseBasic)

parseBasic :: (Num i, RealFloat f) => Parser (Sexpr i f)
parseBasic = lexeme $ try parseList <|> parseAtom

parseSexpr :: (Num i, RealFloat f) => String -> Either ParserError (Sexpr i f)
parseSexpr = parse (between sc eof parseBasic) ""
