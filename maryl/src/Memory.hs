{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Memory
-}

module Memory (Memory, initMemory, updateMemory, readMemory) where

import qualified Data.Map as Map

import Debug.Trace (trace)
import Parsing.ParserAst (Ast (..))

type Memory = Map.Map String Ast

updateMemory :: Memory -> String -> Ast -> Memory
updateMemory mem var value =
    trace ("updating memory with " ++ show var ++ " = " ++ show value) $
        Map.insert var value mem

readMemory :: Memory -> String -> Maybe Ast
readMemory mem symbol =
    Map.lookup symbol mem

initMemory :: Memory
initMemory = Map.empty
