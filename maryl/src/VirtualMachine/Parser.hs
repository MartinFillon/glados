{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Parser
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module VirtualMachine.Parser (parseAssembly) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Int (Int64)
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (..),
    Parsec,
    between,
    choice,
    noneOf,
    parse,
    (<?>),
 )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ParseErrorBundle)
import VirtualMachine.Instructions (
    Op,
    Val (..),
    call,
    jump,
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
parseTrue = lexeme $ string "true" >> return (B True)

parseFalse :: Parser Val
parseFalse = lexeme $ string "false" >> return (B False)

parseChar :: Parser Val
parseChar = lexeme $ C <$> noneOf (" \t\n\r\"" :: [Char])

parseString :: Parser Val
parseString =
    lexeme $
        S
            <$> between
                (char '\"')
                (char '\"')
                ((:) <$> alphaNumChar <*> many alphaNumChar)

parseBool :: Parser Val
parseBool = lexeme (choice [parseTrue, parseFalse]) <?> "Boolean"

parseVal :: Parser Val
parseVal =
    lexeme $
        choice [try parseBool, try parseDigit, try parseString, try parseChar]

parsePush :: Parser Op
parsePush = lexeme (push Nothing <$ lexeme (string "push") <*> parseVal)

parseRet :: Parser Op
parseRet = lexeme (ret Nothing <$ lexeme (string "ret"))

parseCall :: Parser Op
parseCall = lexeme (call Nothing <$ lexeme (string "call"))

parseLabel :: Parser String
parseLabel = lexeme $ (:) <$> char '.' <*> many alphaNumChar

-- parseOp :: String -> Parser a -> Parser a
-- parseOp s p = string

parseLabel' :: Parser (Maybe String)
parseLabel' = choice [try (Just <$> parseLabel), return Nothing]

parseJumpVal' :: Parser (Either Int64 String)
parseJumpVal' = lexeme $ Left <$> parseInt

parseJumpVal'' :: Parser (Either Int64 String)
parseJumpVal'' = lexeme $ Right <$> parseLabel

parseJumpVal :: Parser (Either Int64 String)
parseJumpVal = choice [try parseJumpVal', parseJumpVal'']

parseJumpF :: Parser Op
parseJumpF = lexeme (jumpf Nothing <$ lexeme (string "jumpf") <*> parseJumpVal)

parseJump :: Parser Op
parseJump = lexeme (jump Nothing <$ lexeme (string "jump") <*> parseJumpVal)

parsePushArg :: Parser Op
parsePushArg = lexeme (pushArg Nothing <$ lexeme (string "pushArg") <*> parseInt)

parseKeyWords :: Parser Op
parseKeyWords =
    choice
        [ try parseRet,
          try parsePushArg,
          try parseJumpF,
          try parseJump,
          try parseCall,
          try parsePush
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
parseAssembly = parse (between sc eof (some parseKeyWords)) ""
