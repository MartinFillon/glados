{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Memory
-}
{-# LANGUAGE LambdaCase #-}

module Memory (Memory, addMemory, initMemory, generateUniqueLoopName, updateMemory, readMemory, freeMemory) where

import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Parsing.ParserAst (Ast (..))

type Memory = Map.Map String Ast

updateMemory :: Memory -> String -> Ast -> Memory
updateMemory mem var value =
    Map.insert var value mem

addMemory :: Memory -> String -> Ast -> Either String Memory
addMemory mem var value =
    case readMemory mem var of
        Just _ -> Left ("multiple definition of \"" ++ var ++ "\"")
        Nothing -> Right (updateMemory mem var value)

readMemory :: Memory -> String -> Maybe Ast
readMemory mem symbol =
    Map.lookup symbol mem

generateUniqueLoopName :: Memory -> String
generateUniqueLoopName mem =
    let existingNames = Map.keys mem
        loopNames = filter ("loop" `isPrefixOf`) existingNames
        maxIndex = maximum (0 : map (read . drop 4) loopNames)
     in "loop" ++ show (maxIndex + 1)

freeMemory :: Memory -> Memory
freeMemory =
    Map.filter
        ( \case
            AstDefineFunc _ -> True
            _ -> False
        )

initMemory :: Memory
initMemory = Map.empty
