{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser SExpr
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Parsing.ParserSExpr (
    parseSexpr,
    pOperator,
    pVariable,
    pDigit,
    sexpParser,
    pSExpr,
    lineComment,
    sc,
    handleParseError,
) where

import Data.Void (Void)
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, letterChar, char, alphaNumChar)
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Data.SExpresso.Parse (SExprParser, plainSExprParser, parseSExpr)
import Data.SExpresso.SExpr (SExpr)
import Control.Monad (void)
import Printer (Color(Red, Magenta, Blue), reset, Style (..))

import Control.Exception
import Data.Data (Data)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as E
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void
type SExp = SExpr () String

pOperator :: Parser String
pOperator = some $ oneOf ['+', '-', '*', '<']

pVariable :: Parser String
pVariable = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pDigit :: Parser String
pDigit = some digitChar

convertValue :: Parser String
convertValue = choice
    [ pVariable
    , pOperator
    , pDigit
    ]

sexpParser :: SExprParser Parser () String
sexpParser = plainSExprParser convertValue

pSExpr :: Parser SExp
pSExpr = lexeme $ parseSExpr sexpParser

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseSexpr :: Text -> Either ParserError SExp
parseSexpr = parse (between sc eof pSExpr) ""

addColor :: String -> String
addColor [x] = [x]
addColor ('^':xs) = show Bold ++ show Magenta ++ "^" ++ reset ++ addColor xs
addColor ('|':xs) = show Bold ++ show Blue ++ "|" ++ reset ++ addColor xs
addColor (x:xs) = x : addColor xs
addColor _ = []

handleParseError :: Either ParserError SExp -> IO ()
handleParseError (Right val) = print val
handleParseError (Left (ParseErrorBundle err (PosState input offset source width prefix))) = print errorDisplay
    where
        formattedError = ParseErrorBundle err (PosState (pack (show Bold) <> pack (show Magenta) <> input <> pack reset) offset source width prefix)
        errorDisplay = errorBundlePrettyFormatted formattedError --addColor $ errorBundlePretty formattedError

errorItemLength :: (VisualStream s) => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

errorFancyLength :: (ShowErrorComponent e) => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

errorBundlePrettyFormatted :: ParserError -> String
errorBundlePrettyFormatted ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f :: (TraversableStream s, VisualStream s, ShowErrorComponent e) =>
      (ShowS, PosState s) ->
      ParseError s e ->
      (ShowS, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          "\n"
            <> sourcePosPretty epos
            <> ":\n"
            <> offendingLine
            <> parseErrorTextPretty e
        offendingLine =
          case msline of
            Nothing -> ""
            Just sline ->
              let rpadding =
                    if pointerLen > 0
                      then replicate rpshift ' '
                      else ""
                  pointerLen =
                    if rpshift + elen > slineLen
                      then slineLen - rpshift + 1
                      else elen
                  pointer = replicate pointerLen '^'
                  lineNumber = (show . unPos . sourceLine) epos
                  padding = replicate (length lineNumber + 1) ' '
                  rpshift = unPos (sourceColumn epos) - 1
                  slineLen = length sline
               in padding
                    <> "|\n"
                    <> lineNumber
                    <> " | "
                    <> sline
                    <> "\n"
                    <> padding
                    <> "| "
                    <> rpadding
                    <> pointer
                    <> "\n"
        pxy = Proxy :: Proxy s
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength pxy x
            FancyError _ xs ->
              E.foldl' (\a b -> max a (errorFancyLength b)) 1 xs
