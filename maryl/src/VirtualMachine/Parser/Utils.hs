{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Utils
-}

module VirtualMachine.Parser.Utils (sc, Parser, lexeme) where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, some, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

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
