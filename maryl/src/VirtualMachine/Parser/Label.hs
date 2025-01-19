{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Label
-}

module VirtualMachine.Parser.Label (parseLabel) where

import Text.Megaparsec (many, noneOf)
import Text.Megaparsec.Char (char)
import VirtualMachine.Parser.Utils (Parser, lexeme)

parseLabel :: Parser String
parseLabel = lexeme $ (:) <$> char '.' <*> many (noneOf " \t\n")
