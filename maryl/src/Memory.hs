{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Memory
-}
{-# LANGUAGE LambdaCase #-}

module Memory (Memory, addMemory, initMemory, generateUniqueElseName, generateUniqueLoopName, updateMemory, readMemory, freeMemory) where

import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Parsing.ParserAst (Ast (..))

type Memory = Map.Map String Ast

updateMemory :: Memory -> String -> Ast -> Memory
updateMemory mem var value =
    trace ("updating mem with " ++ show var ++ " = " ++ show value) $
    Map.insert var value mem

addMemory :: Memory -> String -> Ast -> Either String Memory
addMemory mem var value =
    trace ("adding " ++ show var ++ " to mem (" ++ show value ++ ")") $
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
     in "loop" ++ show (maxIndex + 1 :: Integer)

generateUniqueElseName :: Memory -> String
generateUniqueElseName mem =
    let existingNames = Map.keys mem
        elseNames = filter ("else" `isPrefixOf`) existingNames
        maxIndex = maximum (0 : map (read . drop 4) elseNames)
     in "else" ++ show (maxIndex + 1 :: Integer)

freeMemory :: Memory -> Memory
freeMemory =
    Map.filter
        ( \case
            AstDefineFunc _ -> True
            AstDefineLoop {} -> True
            AstIf {} -> True
            AstGlobal _ -> True
            _ -> False
        )

initMemory :: Memory
initMemory = Map.empty
