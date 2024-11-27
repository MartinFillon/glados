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
    handleParseError,
) where

import Data.Void (Void)
import Text.Megaparsec
    ( empty,
      (<|>),
      (<?>),
      parse,
      between,
      choice,
      some,
      Parsec,
      MonadParsec(eof),
      ParseErrorBundle, try, optional )
import Text.Megaparsec.Char (digitChar, letterChar, char, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.SExpresso.Parse (SExprParser, plainSExprParser, parseSExpr)
import Data.SExpresso.SExpr (SExpr)
import Control.Monad (void)
import Parsing.ErrorBundlePretty (errorBundlePrettyFormatted)

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void
type SExp = SExpr () String

pOperator :: Parser String
pOperator = choice
    [ "+"
    , "-"
    , "*"
    , "<"
    , "eq?"
    , "if"
    , "div"
    , "mod"
    ]

pVariable :: Parser String
pVariable = (:) <$> letterChar <*> some alphaNumChar <?> "variable"

pDigit :: Parser String
pDigit = do
    sign <- optional (char '+' <|> char '-')
    n <- some digitChar
    case sign of
        Nothing -> return n
        Just s -> return (s:n)

pBool :: Parser String
pBool = choice ["#f", "#t"]

convertValue :: Parser String
convertValue = choice
    [ try pDigit
    , pOperator
    , try pBool
    , try pVariable
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

parseSexpr :: String -> Either ParserError SExp
parseSexpr = parse (between sc eof pSExpr) ""

handleParseError :: Bool -> Either ParserError SExp -> IO ()
handleParseError _ (Right val) = print val
handleParseError showColors (Left err) = putStr $ errorBundlePrettyFormatted showColors err
