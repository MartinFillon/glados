{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Instructions
-}

module VirtualMachine.Parser.Instructions (parseKeyWords) where

import Text.Megaparsec (MonadParsec (try), choice, optional)
import Text.Megaparsec.Char (string)
import VirtualMachine.Instructions (
    Instruction,
    call,
    dup,
    get,
    jump,
    jumpf,
    load,
    noop,
    push,
    pushArg,
    ret,
    void,
 )
import VirtualMachine.Parser.Label (parseLabel)
import VirtualMachine.Parser.Utils (Parser, lexeme)
import VirtualMachine.Parser.Values (
    parseInt,
    parseJumpVal,
    parseString',
    parseVal,
 )

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

parsePush :: Parser Instruction
parsePush = lexeme (parseInstruction push "push" parseVal)

parseRet :: Parser Instruction
parseRet = lexeme (parseInstruction' ret "ret")

parseDup :: Parser Instruction
parseDup = lexeme (parseInstruction' dup "dup")

parseVoid :: Parser Instruction
parseVoid = lexeme (parseInstruction' void "void")

parseNoop :: Parser Instruction
parseNoop = lexeme (parseInstruction' noop "noop")

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
      parseLoad,
      parseNoop,
      parseDup,
      parseVoid
    ]

parseKeyWords :: Parser Instruction
parseKeyWords = choice $ map try keyWords
