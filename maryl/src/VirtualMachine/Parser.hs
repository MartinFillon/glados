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
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (..),
    Parsec,
    between,
    choice,
    noneOf,
    parse,
    sepBy,
    (<?>),
 )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ParseErrorBundle)
import VirtualMachine.Instructions (
    Instruction,
    Value (..),
    call,
    get,
    jump,
    jumpf,
    load,
    push,
    pushArg,
    ret,
 )

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sce :: Parser ()
sce = L.space empty empty empty

parseInt :: Parser Int
parseInt = lexeme (L.signed sce L.decimal)

parseFloat :: Parser Value
parseFloat = lexeme $ D <$> L.signed sce L.float

parseDigit :: Parser Value
parseDigit = lexeme $ N <$> L.signed sce L.decimal

parseTrue :: Parser Value
parseTrue = lexeme $ string "true" >> return (B True)

parseFalse :: Parser Value
parseFalse = lexeme $ string "false" >> return (B False)

-- parseChar :: Parser Value
-- parseChar = lexeme $ C <$> noneOf (" \t\n\r\"[]" :: [Char])

parseString' :: Parser String
parseString' =
    lexeme $
        between
            (char '\"')
            (char '\"')
            ((:) <$> noneOf ("\"" :: [Char]) <*> many (noneOf ("\"" :: [Char])))

parseChar' :: Parser Char
parseChar' =
    lexeme $
        between
            (char '\'')
            (char '\'')
            (noneOf ("\'" :: [Char]))

parseChar :: Parser Value
parseChar = lexeme $ C <$> parseChar'

parseString :: Parser Value
parseString =
    lexeme $ S <$> parseString'

parseBool :: Parser Value
parseBool = lexeme (choice [parseTrue, parseFalse]) <?> "Boolean"

parseList :: Parser Value
parseList =
    L
        <$> between (char '[') (char ']') (parseVal `sepBy` lexeme ",")

parseVal :: Parser Value
parseVal =
    lexeme $
        choice
            [ try parseList,
              try parseFloat,
              try parseBool,
              try parseDigit,
              try parseChar,
              try parseString
            ]

parseLabel :: Parser String
parseLabel = lexeme $ (:) <$> char '.' <*> many alphaNumChar

parseInstruction ::
    (Maybe String -> a -> Instruction) -> String -> Parser a -> Parser Instruction
parseInstruction f s p = (\l _ v -> f l v) <$> optional parseLabel <*> lexeme (string s) <*> p

parseInstruction' ::
    (Maybe String -> Instruction) -> String -> Parser Instruction
parseInstruction' f s = (\l _ -> f l) <$> optional parseLabel <*> lexeme (string s)

parseInstruction2A ::
    (Maybe String -> a -> b -> Instruction) ->
    String ->
    Parser a ->
    Parser b ->
    Parser Instruction
parseInstruction2A f s pa pb =
    (\l _ a b -> f l a b)
        <$> optional parseLabel
        <*> lexeme (string s)
        <*> pa
        <*> pb

parseJumpVal' :: Parser (Either Int String)
parseJumpVal' = lexeme $ Left <$> parseInt

parseJumpVal'' :: Parser (Either Int String)
parseJumpVal'' = lexeme $ Right <$> parseLabel

parseJumpVal :: Parser (Either Int String)
parseJumpVal = choice [try parseJumpVal', parseJumpVal'']

parsePush :: Parser Instruction
parsePush = lexeme (parseInstruction push "push" parseVal)

parseRet :: Parser Instruction
parseRet = lexeme (parseInstruction' ret "ret")

parseCall :: Parser Instruction
parseCall = lexeme (parseInstruction call "call" parseString')

parseJumpF :: Parser Instruction
parseJumpF = lexeme (parseInstruction jumpf "jumpf" parseJumpVal)

parseJump :: Parser Instruction
parseJump = lexeme (parseInstruction jump "jump" parseJumpVal)

parsePushArg :: Parser Instruction
parsePushArg = lexeme (parseInstruction pushArg "pushArg" parseInt)

parseGet :: Parser Instruction
parseGet = lexeme (parseInstruction get "get" parseString')

parseLoad :: Parser Instruction
parseLoad = lexeme (parseInstruction2A load "load" parseString' parseVal)

keyWords :: [Parser Instruction]
keyWords =
    [ parseRet,
      parsePushArg,
      parseJumpF,
      parseJump,
      parseCall,
      parsePush,
      parseGet,
      parseLoad
    ]

parseKeyWords :: Parser Instruction
parseKeyWords = choice $ map try keyWords

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc =
    L.space
        (void $ some (char ' ' <|> char '\t' <|> char '\r' <|> char '\n'))
        lineComment
        empty

parseAssembly :: String -> Either ParserError [Instruction]
parseAssembly = parse (between sc eof (some parseKeyWords)) ""
