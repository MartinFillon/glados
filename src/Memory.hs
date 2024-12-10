{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Memory
-}

module Memory (Memory, initMemory, updateMemory, readMemory) where

import qualified Data.Map as Map

import Debug.Trace (trace)
import Parsing.SExprToAst (Ast (..))

type Memory = Map.Map String Ast

updateMemory :: Memory -> String -> Ast -> Memory
updateMemory mem var value =
    -- trace ("add " ++ var ++ " to memory with: " ++ show value) $
    Map.insert var value mem

readMemory :: Memory -> String -> Maybe Ast
readMemory mem symbol =
    -- trace ("reading " ++ symbol ++ " from memory") $
    Map.lookup symbol mem

initMemory :: Memory
initMemory = Map.empty
