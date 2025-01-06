{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Memory
-}
{-# LANGUAGE LambdaCase #-}

module Memory (Memory, initMemory, updateMemory, readMemory, freeMemory) where

import qualified Data.Map as Map

import Debug.Trace (trace)
import Parsing.ParserAst (Ast (..))

type Memory = Map.Map String Ast

updateMemory :: Memory -> String -> Ast -> Memory
updateMemory mem var value = trace ("updating: " ++ show var ++ " = " ++ show value ++ "!") $
    Map.insert var value mem

readMemory :: Memory -> String -> Maybe Ast
readMemory mem symbol =
    Map.lookup symbol mem

freeMemory :: Memory -> Memory
freeMemory =
    Map.filter
        ( \case
            AstDefineFunc _ -> True
            _ -> False
        )

initMemory :: Memory
initMemory = Map.empty
