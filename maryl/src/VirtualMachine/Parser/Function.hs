{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Function
-}

module VirtualMachine.Parser.Function (parseFunction) where

import Control.Monad (void)
import Text.Megaparsec (many)
import Text.Megaparsec.Char (string)
import VirtualMachine.Instructions (Instruction)
import VirtualMachine.Parser.Instructions (parseKeyWords)
import VirtualMachine.Parser.Utils (Parser, lexeme)
import VirtualMachine.Parser.Values (parseString')

parseFunctionHeader :: Parser String
parseFunctionHeader = do
    _ <- lexeme $ string ".header_function"
    lexeme parseString'

parseFunctionFooter :: Parser ()
parseFunctionFooter = void $ lexeme $ string ".footer_function"

parseFunction :: Parser (String, [Instruction])
parseFunction = do
    name <- parseFunctionHeader
    body <- many parseKeyWords
    parseFunctionFooter
    return (name, body)
