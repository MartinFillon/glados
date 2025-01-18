{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Assignment
-}

module Eval.Structures (evalFinalStruct, normalizeStruct) where

import Data.List (find)
import Debug.Trace (trace)
import Memory (readMemory)
import Parsing.ParserAst (Ast (..), MarylType (..), Structure (..), Variable (..), isValidType)

matchPositionalFields :: [(String, MarylType, Ast)] -> [Ast] -> Either String [Ast]
matchPositionalFields defFields newFields
    | length newFields == length defFields =
        Right (zipWith (\(name, _, _) value -> AstLabel name value) defFields newFields)
    | otherwise =
        Left
            ( "Defining a structure requires no uninitialised values, expected "
                ++ show (length defFields)
                ++ " elements but got "
                ++ show (length newFields)
                ++ "."
            )

validateField :: [Ast] -> (String, MarylType, Ast) -> Either String Ast
validateField labeledFields (name, expectedType, defaultValue) =
    case find (\(AstLabel n _) -> n == name) labeledFields of
        Just (AstLabel _ value) ->
            if isValidType value expectedType
                then Right (AstLabel name value)
                else
                    Left
                        ( "Type mismatch for field '"
                            ++ name
                            ++ "', expected "
                            ++ show expectedType
                            ++ " but got \""
                            ++ show value
                            ++ "\"."
                        )
        _ -> Right (AstLabel name defaultValue)

mergeFields :: [(String, MarylType, Ast)] -> Either String [Ast] -> Either String [Ast]
mergeFields defFields =
    either Left ( \labeledFields -> traverse (validateField labeledFields) defFields)

normalizeStruct :: Ast -> Ast -> Either String Ast
normalizeStruct (AstDefineStruct (Structure _ structProps)) (AstStruct instanceFields) =
    let definedFields =
            map
                ( \(AstDefineVar (Variable name varType defaultValue)) ->
                    (name, varType, defaultValue)
                )
                structProps
        labeledFields = case instanceFields of
            (AstLabel _ _ : _) -> Right instanceFields
            _ -> matchPositionalFields definedFields instanceFields

        validatedFields = mergeFields definedFields labeledFields
     in case validatedFields of
        Right normalized -> evalFinalStruct normalized (AstStruct normalized)
        Left err -> Left err
normalizeStruct (AstDefineStruct (Structure _ structProps)) AstVoid =
    let definedFields =
            map
                ( \(AstDefineVar (Variable name varType defaultValue)) ->
                    (name, varType, defaultValue)
                )
                structProps
        defaultLabeledFields = Right (map (\(name, _, defaultValue) -> AstLabel name defaultValue) definedFields)

        validatedFields = mergeFields definedFields defaultLabeledFields
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
