{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Values
-}
{-# LANGUAGE OverloadedStrings #-}

module VirtualMachine.Parser.Values (
    parseJumpVal,
    parseInt,
    parseFloat,
    parseDigit,
    parseString',
    parseChar',
    parseChar,
    parseVal,
    parseString,
    parseBool,
    parseList,
) where

import Control.Monad (void)
import qualified Data.Map as Map
import Text.Megaparsec (
    between,
    choice,
    empty,
    many,
    noneOf,
    sepBy,
    try,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L
import VirtualMachine.Instructions (Value (..))
import VirtualMachine.Parser.Label (parseLabel)
import VirtualMachine.Parser.Utils (Parser, lexeme)

parseJumpVal' :: Parser (Either Int String)
parseJumpVal' = lexeme $ Left <$> parseInt

parseJumpVal'' :: Parser (Either Int String)
parseJumpVal'' = lexeme $ Right <$> parseLabel

parseJumpVal :: Parser (Either Int String)
parseJumpVal = choice [try parseJumpVal', parseJumpVal'']

sce :: Parser ()
sce = L.space empty empty empty

parseInt :: Parser Int
parseInt = lexeme (L.signed sce L.decimal)

parseFloat :: Parser Value
parseFloat = lexeme $ D <$> L.signed sce L.float

parseDigit :: Parser Value
parseDigit = lexeme $ N <$> L.signed sce L.decimal

parseTrue :: Parser Value
parseTrue = lexeme $ string "True" >> return (B True)

parseFalse :: Parser Value
parseFalse = lexeme $ string "False" >> return (B False)

parseString' :: Parser String
parseString' =
    lexeme $
        between
            (char '\"')
            (char '\"')
            ( (:)
                <$> (try parseEscapedChar <|> noneOf ("\"" :: [Char]))
                <*> many (try parseEscapedChar <|> noneOf ("\"" :: [Char]))
            )

parseChar' :: Parser Char
parseChar' =
    lexeme $
        between
            (char '\'')
            (char '\'')
            (try parseEscapedChar <|> noneOf ("\'" :: [Char]))

parseChar :: Parser Value
parseChar = lexeme $ C <$> parseChar'

parseEscapedChar :: Parser Char
parseEscapedChar =
    choice
        [ try (string "\\\"" >> return '\"'),
          try (string "\\n" >> return '\n'),
          try (string "\\r" >> return '\r'),
          try (string "\\t" >> return '\t'),
          try (string "\\\\" >> return '\\')
        ]

parseString :: Parser Value
parseString =
    lexeme $ S <$> parseString'

parseBool :: Parser Value
parseBool = lexeme (choice [parseTrue, parseFalse]) <?> "Boolean"

parseList :: Parser Value
parseList =
    L
        <$> between (char '[') (char ']') (parseVal `sepBy` lexeme ",")

parseStruct' :: Parser (String, Value)
parseStruct' = (,) <$> parseString' <*> (void (char '=') >> parseVal)

parseStruct :: Parser Value
parseStruct =
    St . Map.fromList
        <$> between (char '{') (char '}') (many parseStruct')
        <?> "Structure"

parseVal :: Parser Value
parseVal =
    lexeme $
        choice
            [ try parseList,
              try parseFloat,
              try parseBool,
              try parseDigit,
              try parseChar,
              try parseString,
              try parseStruct
            ]
