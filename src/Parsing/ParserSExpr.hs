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
    lineComment,
    sc,
    handleParseError,
) where

import Control.Applicative (Alternative (many))
import Control.Monad (void)
import Data.SExpresso.SExpr (SExpr)
import Data.Void (Void)
import Parsing.ErrorBundlePretty (errorBundlePrettyFormatted)
import Text.Megaparsec (
    MonadParsec (eof),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    empty,
    optional,
    parse,
    some,
    try,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void
type SExp = SExpr () String

pOperator :: Parser String
pOperator =
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

pVariable :: Parser String
pVariable = (:) <$> letterChar <*> some alphaNumChar <?> "variable"

pDigit :: Parser String
pDigit = do
    sign <- optional (char '+' <|> char '-')
    n <- some digitChar
    case sign of
        Nothing -> return n
        Just s -> return (s : n)

pFloat :: Parser String
pFloat = do
    sign <- optional (char '+' <|> char '-')
    n <- some digitChar
    _ <- char '.'
    m <- some digitChar
    case sign of
        Nothing -> return (n ++ "." ++ m)
        Just s -> return (s : n ++ "." ++ m)

pBool :: Parser String
pBool = choice ["#f", "#t"]

convertValue :: Parser String
convertValue =
    choice
        [ try pDigit,
          pOperator,
          try pBool,
          try pVariable
        ]

convertValue' :: Parser String
convertValue' = try pFloat <|> try pDigit <|> try pOperator <|> try pBool <|> pVariable

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

data Sexpr = Atom String | List [Sexpr] deriving (Show)

parseAtom :: Parser Sexpr
parseAtom = Atom <$> convertValue'

parseList :: Parser Sexpr
parseList = lexeme $ List <$> between (char '(') (char ')') (many parseBasic)

parseBasic :: Parser Sexpr
parseBasic = lexeme $ try parseList <|> parseAtom

parseSexpr :: String -> Either ParserError Sexpr
parseSexpr = parse (between sc eof parseBasic) ""

handleParseError :: Show a => Bool -> Either ParserError a -> IO ()
handleParseError _ (Right val) = print val
handleParseError showColors (Left err) = putStr $ errorBundlePrettyFormatted showColors err
