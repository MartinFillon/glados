{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ErrorBundlePretty (
    ErrorItem (..),
    ErrorFancy (..),
    ParseError (..),
    errorOffset,
    ParseErrorBundle (..),

    errorBundlePrettyFormatted,
    errorItemLength,
    errorFancyLength,
  )
where

import safe Data.Proxy ( Proxy(..) )
import qualified Data.Set as E
import safe Text.Megaparsec.Pos
    ( sourcePosPretty, unPos, SourcePos(sourceColumn, sourceLine) )
import safe Text.Megaparsec.Stream
    ( Stream(Token),
      TraversableStream(reachOffset),
      VisualStream(tokensLength) )
import safe Text.Megaparsec.State ( PosState(pstateSourcePos) )
import Printer (Color(Red), reset, Style (Bold), getColorsFromConf)
import safe Text.Megaparsec.Error
    ( errorOffset,
      showErrorItem,
      ErrorFancy(..),
      ErrorItem(..),
      ParseError(..),
      ParseErrorBundle(..),
      ShowErrorComponent(..) )
import Data.Maybe (isNothing, fromJust)
import Data.Set (Set)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List (intercalate)

errorBundlePrettyFormatted ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  Bool ->
  ParseErrorBundle s e ->
  IO String
errorBundlePrettyFormatted showColors bundle = do
  colors <- getColorsFromConf
  if isNothing colors
    then return $ errorBundlePrettyFormatted' False (Red,Red,Red) bundle
    else return $ errorBundlePrettyFormatted' showColors (fromJust colors) bundle

errorBundlePrettyFormatted' ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  Bool ->
  (Color, Color, Color) ->
  ParseErrorBundle s e ->
  String
errorBundlePrettyFormatted' showColors (cWarning, cError, cInfo) ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f :: (ShowS, PosState s) -> ParseError s e -> (ShowS, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          "\n"
            <> (if showColors then show Bold <> sourcePosPretty epos else sourcePosPretty epos)
            <> ":\n"
            <> (if showColors then parseErrorTextPrettyFormatted e <> reset else parseErrorTextPrettyFormatted e)
            <> offendingLine
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
                    <> (if showColors then show Bold <> show cInfo <> "|\n" else "|\n")
                    <> lineNumber
                    <> (if showColors then " | " <> reset else " | ")
                    <> (if showColors then show Bold <> show cWarning <> sline <> reset else sline)
                    <> "\n"
                    <> padding
                    <> (if showColors then show Bold <> show cInfo <> "| " <> reset else "| ")
                    <> rpadding
                    <> (if showColors then show Bold <> show cError <> pointer <> reset else pointer)
                    <> "\n"
        pxy = Proxy :: Proxy s
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength pxy x
            FancyError _ xs ->
              E.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

errorItemLength :: (VisualStream s) => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

errorFancyLength :: (ShowErrorComponent e) => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

parseErrorTextPrettyFormatted ::
  forall s e.
  (VisualStream s, ShowErrorComponent e) =>
  ParseError s e ->
  String
parseErrorTextPrettyFormatted (TrivialError _ us ps) =
  if isNothing us && E.null ps
    then "unknown parse error\n"
    else
      messageItemsPretty "unexpected " (showErrorItem pxy `E.map` maybe E.empty E.singleton us)
        <> messageItemsPretty "" (showErrorItem pxy `E.map` ps)
  where
    pxy = Proxy :: Proxy s
parseErrorTextPrettyFormatted (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error\n"
    else unlines (showErrorFancy <$> E.toAscList xs)

messageItemsPretty :: String -> Set String -> String
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
      prefix <> (orList . NE.fromList . E.toAscList) ts <> "\n"

orList :: NonEmpty String -> String
orList (x :| []) = x
orList (x :| [y]) = x <> " or " <> y
orList xs = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

showErrorFancy :: (ShowErrorComponent e) => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got "
      <> show (unPos actual)
      <> ", should be "
      <> p
      <> show (unPos ref)
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a
