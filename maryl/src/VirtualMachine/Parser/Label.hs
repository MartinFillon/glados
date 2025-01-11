{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Label
-}

module VirtualMachine.Parser.Label (parseLabel) where

import Text.Megaparsec (many)
import Text.Megaparsec.Char (alphaNumChar, char)
import VirtualMachine.Parser.Utils (Parser, lexeme)

parseLabel :: Parser String
parseLabel = lexeme $ (:) <$> char '.' <*> many alphaNumChar
