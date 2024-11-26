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
    pVariable,
    pDigit,
    sexpParser,
    pSExpr,
    lineComment,
    sc,
) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
    ( empty,
      (<|>),
      oneOf,
      parse,
      between,
      choice,
      some,
      Parsec,
      MonadParsec(eof),
      ParseErrorBundle, (<?>), many )
import Text.Megaparsec.Char (digitChar, letterChar, char, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.SExpresso.Parse (SExprParser, plainSExprParser, parseSExpr)
import Data.SExpresso.SExpr (SExpr)
import Control.Monad (void)

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void
type SExp = SExpr () String

pOperator :: Parser String
pOperator = some $ oneOf ['+', '-', '*', '<']

pVariable :: Parser String
pVariable = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pDigit :: Parser String
pDigit = some digitChar

convertValue :: Parser String
convertValue = choice
    [ pVariable
    , pOperator
    , pDigit
    ]

sexpParser :: SExprParser Parser () String
sexpParser = plainSExprParser convertValue

pSExpr :: Parser SExp
pSExpr = lexeme $ parseSExpr sexpParser

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseSexpr :: Text -> Either ParserError SExp
parseSexpr = parse (between sc eof pSExpr) ""
