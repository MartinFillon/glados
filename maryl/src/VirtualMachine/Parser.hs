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
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (..),
    Parsec,
    between,
    choice,
    failure,
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sce :: Parser ()
sce = L.space empty empty empty

parseInt :: Parser Int64
parseInt = lexeme (L.signed sce L.decimal)

parseFloat :: Parser Val
parseFloat = lexeme $ D <$> L.signed sce L.float

parseDigit :: Parser Val
parseDigit = lexeme $ N <$> L.signed sce L.decimal

parseTrue :: Parser Val
parseTrue = lexeme $ string "true" >> return (B True)

parseFalse :: Parser Val
parseFalse = lexeme $ string "false" >> return (B False)

parseChar :: Parser Val
parseChar = lexeme $ C <$> noneOf (" \t\n\r\"[]" :: [Char])

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

isN :: Val -> Bool
isN (N _) = True
isN _ = False

isB :: Val -> Bool
isB (B _) = True
isB _ = False

isD :: Val -> Bool
isD (D _) = True
isD _ = False

isS :: Val -> Bool
isS (S _) = True
isS _ = False

isC :: Val -> Bool
isC (C _) = True
isC _ = False

isL :: Val -> Bool
isL (L _) = True
isL _ = False

verifyList :: Val -> (Val -> Bool) -> Bool
verifyList (L x) f = all f x
verifyList _ _ = False

getVerifier :: Val -> (Val -> Bool)
getVerifier (N _) = isN
getVerifier (D _) = isD
getVerifier (S _) = isS
getVerifier (C _) = isC
getVerifier (B _) = isB
getVerifier (L []) = const True
getVerifier (L (x : _)) = \x' -> isL x' && verifyList x' (getVerifier x)

isSameType :: [Val] -> Parser [Val]
isSameType [] = pure []
isSameType l@(x : _)
    | all (getVerifier x) l = pure l
    | otherwise = failure Nothing (Set.fromList [])

parseList :: Parser Val
parseList =
    L
        <$> ( between (char '[') (char ']') (parseVal `sepBy` lexeme ",")
                >>= isSameType
            )

parseVal :: Parser Val
parseVal =
    lexeme $
        choice
            [ try parseList,
              try parseFloat,
              try parseBool,
              try parseDigit,
              try parseString,
              try parseChar
            ]

parseLabel :: Parser String
parseLabel = lexeme $ (:) <$> char '.' <*> many alphaNumChar

parseOp :: (Maybe String -> a -> Op) -> String -> Parser a -> Parser Op
parseOp f s p = (\l _ v -> f l v) <$> optional parseLabel <*> lexeme (string s) <*> p

parseOp' :: (Maybe String -> Op) -> String -> Parser Op
parseOp' f s = (\l _ -> f l) <$> optional parseLabel <*> lexeme (string s)

parseJumpVal' :: Parser (Either Int64 String)
parseJumpVal' = lexeme $ Left <$> parseInt

parseJumpVal'' :: Parser (Either Int64 String)
parseJumpVal'' = lexeme $ Right <$> parseLabel

parseJumpVal :: Parser (Either Int64 String)
parseJumpVal = choice [try parseJumpVal', parseJumpVal'']

parsePush :: Parser Op
parsePush = lexeme (parseOp push "push" parseVal)

parseRet :: Parser Op
parseRet = lexeme (parseOp' ret "ret")

parseCall :: Parser Op
parseCall = lexeme (parseOp' call "call")

parseJumpF :: Parser Op
parseJumpF = lexeme (parseOp jumpf "jumpf" parseJumpVal)

parseJump :: Parser Op
parseJump = lexeme (parseOp jump "jump" parseJumpVal)

parsePushArg :: Parser Op
parsePushArg = lexeme (parseOp pushArg "pushArg" parseInt)

keyWords :: [Parser Op]
keyWords = [parseRet, parsePushArg, parseJumpF, parseJump, parseCall, parsePush]

parseKeyWords :: Parser Op
parseKeyWords = choice $ map try keyWords

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc =
    L.space
        (void $ some (char ' ' <|> char '\t' <|> char '\r' <|> char '\n'))
        lineComment
        empty

parseAssembly :: String -> Either ParserError [Op]
parseAssembly = parse (between sc eof (some parseKeyWords)) ""
