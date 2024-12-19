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

import Control.Applicative (Alternative (..), optional)
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

parseLabel :: Parser String
parseLabel = lexeme $ (:) <$> char '.' <*> many alphaNumChar

parseOp :: (Maybe String -> a -> Op) -> String -> Parser a -> Parser Op
parseOp f s p = (\l _ v -> f l v) <$> optional parseLabel <*> lexeme (string s) <*> p

parseJumpVal' :: Parser (Either Int64 String)
parseJumpVal' = lexeme $ Left <$> parseInt

parseJumpVal'' :: Parser (Either Int64 String)
parseJumpVal'' = lexeme $ Right <$> parseLabel

parseJumpVal :: Parser (Either Int64 String)
parseJumpVal = choice [try parseJumpVal', parseJumpVal'']

parsePush :: Parser Op
parsePush = lexeme (parseOp push "push" parseVal)

parseRet :: Parser Op
parseRet = lexeme (parseOp (\l _ -> ret l) "ret" (pure ()))

parseCall :: Parser Op
parseCall = lexeme (parseOp (\l _ -> call l) "call" (pure ()))

parseJumpF :: Parser Op
parseJumpF = lexeme (parseOp jumpf "jumpf" parseJumpVal)

parseJump :: Parser Op
parseJump = lexeme (parseOp jump "jump" parseJumpVal)

parsePushArg :: Parser Op
parsePushArg = lexeme (parseOp pushArg "pushArg" parseInt)

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
