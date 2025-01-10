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
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (..),
    between,
    parse,
 )
import Text.Megaparsec.Error (ParseErrorBundle)
import VirtualMachine.Instructions (
    Instruction,
 )
import VirtualMachine.Parser.Function (parseFunction)
import VirtualMachine.Parser.Instructions (parseKeyWords)
import VirtualMachine.Parser.Utils (Parser, sc)

type ParserError = ParseErrorBundle String Void

parseData :: Parser (Either Instruction (String, [Instruction]))
parseData = try (Right <$> parseFunction) <|> (Left <$> parseKeyWords)

parseAssembly ::
    String ->
    Either ParserError [Either Instruction (String, [Instruction])]
parseAssembly = parse (between sc eof (some parseData)) ""
