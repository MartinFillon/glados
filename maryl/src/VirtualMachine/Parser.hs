{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Parser
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module VirtualMachine.Parser (parseAssembly) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Int (Int64)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), Parsec, between, choice, parse)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)
import VirtualMachine.Instructions (
    Op,
    Val (..),
    call,
    jumpf,
    push,
    pushArg,
    ret,
 )

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

sce :: Parser ()
sce = L.space empty empty empty

parseInt :: Parser Int64
parseInt = lexeme (L.signed sce L.decimal)

parseDigit :: Parser Val
parseDigit = lexeme $ N <$> L.signed sce L.decimal

parseTrue :: Parser Val
parseTrue = string "true" >> return (B True)

parseFalse :: Parser Val
parseFalse = string "false" >> return (B False)

parseBool :: Parser Val
parseBool = choice [parseTrue, parseFalse]

parseVal :: Parser Val
parseVal = choice [parseBool, parseDigit]

parsePush :: Parser Op
parsePush = lexeme (push <$ lexeme (string "push") <*> parseVal)

parseRet :: Parser Op
parseRet = lexeme (ret <$ lexeme (string "ret"))

parseCall :: Parser Op
parseCall = lexeme (call <$ lexeme (string "call"))

parseJump :: Parser Op
parseJump = lexeme (jumpf <$ lexeme (string "jumpf") <*> parseInt)

parsePushArg :: Parser Op
parsePushArg = lexeme (pushArg <$ lexeme (string "pushArg") <*> parseInt)

parseKeyWords :: Parser Op
parseKeyWords =
    choice
        [ try parseRet,
          try parsePush,
          try parseJump,
          try parseCall,
          try parsePushArg
        ]

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc =
    L.space
        (void $ some (char ' ' <|> char '\t' <|> char '\r' <|> char '\n'))
        lineComment
        empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseAssembly :: String -> Either ParserError [Op]
parseAssembly = parse (between sc eof (many parseKeyWords)) ""
