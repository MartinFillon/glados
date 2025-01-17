{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Assignment
-}

module Eval.Assignment (updateList, normalizeStruct, evalFinalStruct) where

import Debug.Trace (trace)
import Data.List (find)
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..), MarylType (..), Variable (..), Structure (..), isValidType)

changeAtIdx :: Ast -> [Int] -> Ast -> Ast
changeAtIdx (AstList elements) [idx] newVal
    | idx >= 0 && idx < length elements =
        AstList (take idx elements ++ [newVal] ++ drop (idx + 1) elements)
    | otherwise = AstList elements
changeAtIdx (AstList elements) (x : xs) newVal
    | x >= 0 && x < length elements =
        let current = elements !! x
            updated = changeAtIdx current xs newVal
         in AstList (take x elements ++ [updated] ++ drop (x + 1) elements)
    | otherwise = AstList elements
changeAtIdx ast _ _ = ast

updateList :: String -> Ast -> Memory -> Ast -> Either String (Ast, Memory)
updateList listName (AstListElem _ idxs) mem newVal =
    case readMemory mem listName of
        Just (AstList elements) -> Right (changeAtIdx (AstList elements) idxs newVal, mem)
        _ -> Left ("Unable to update " ++ show (AstListElem listName idxs) ++ " with " ++ show newVal ++ ".")
updateList _ ast mem _ = Right (ast, mem)

----- Structures

matchPositionalFields :: [(String, MarylType, Ast)] -> [Ast] -> Either String [Ast]
matchPositionalFields defFields newFields
    | length newFields == length defFields =
        Right (zipWith (\(name, _, _) value -> AstLabel name value) defFields newFields)
    | otherwise = Left ("Defining a structure requires no uninitialised values, expected " ++ show (length defFields) ++
        " elements but got " ++ show (length newFields) ++ ".")

validateField :: [Ast] -> (String, MarylType, Ast) -> Either String Ast
validateField labeledFields (name, expectedType, defaultValue) =
    case find (\(AstLabel n _) -> n == name) labeledFields of
        Just (AstLabel _ value) ->
            if isValidType value expectedType
            then Right (AstLabel name value)
            else Left ("Type mismatch for field '" ++ name ++ "', expected " ++ show expectedType ++ " but got \"" ++
                show value ++ "\".")
        _ -> Right (AstLabel name defaultValue)

validateAndNormalizeFields :: [(String, MarylType, Ast)] -> Either String [Ast] -> Either String [Ast]
validateAndNormalizeFields defFields labelFields = case labelFields of
    Right labeledFields -> traverse (validateField labeledFields) defFields
    Left err -> Left err

normalizeStruct :: Ast -> Ast -> Either String Ast
normalizeStruct (AstDefineStruct (Structure _ structProps)) (AstStruct instanceFields) =
    let definedFields = map (\(AstDefineVar (Variable name varType defaultValue)) ->
            (name, varType, defaultValue)) structProps
        labeledFields = case instanceFields of
            (AstLabel _ _ : _) -> Right instanceFields
            _ -> matchPositionalFields definedFields instanceFields

        validatedFields = validateAndNormalizeFields definedFields labeledFields
     in case validatedFields of
            Right normalized -> evalFinalStruct normalized (AstStruct normalized)
            Left err -> Left err
normalizeStruct _ _ = Left "Invalid struct definition or instance."

evalFinalStruct :: [Ast] -> Ast -> Either String Ast -- check if there are extra values
evalFinalStruct ((AstLabel label AstVoid) : _) _ =
    Left ("Defining a structure requires no uninitialised values, expected value for field '" ++ label ++ "'.")
evalFinalStruct ((AstLabel _ _) : xs) ast = evalFinalStruct xs ast
evalFinalStruct [] ast = Right ast
evalFinalStruct _ ast = Right ast
