{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Assignment
-}

module Eval.Structures (evalFinalStruct, normalizeStruct) where

import Data.List (find)
import Debug.Trace (trace)
import Eval.Lists (checkListType, getAtIdx, getIndexes)
import Memory (Memory, readMemory)
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

exhaustTypes :: String -> MarylType -> Ast -> Memory -> Either String Ast
exhaustTypes name expectedType (AstListElem var idxs) mem =
    case readMemory mem var of
        Just (AstList eles) ->
            getIndexes mem idxs >>= \idxs' -> case getAtIdx (AstList eles) idxs' of
                Right ast ->
                    if isValidType ast expectedType
                        then Right ast
                        else Left (show ast)
        _ -> Left "invalid list call."
exhaustTypes name expectedType (AstArg ast _) mem = exhaustTypes name expectedType ast mem
exhaustTypes name expectedType (AstVar var) mem =
    maybe (Left var) (\val -> exhaustTypes name expectedType val mem) (readMemory mem var) 
exhaustTypes name expectedType ast mem
    | isValidType ast expectedType = Right ast
    | otherwise = Left (show ast)

validateField :: [Ast] -> (String, MarylType, Ast) -> Memory -> Either String Ast
validateField labeledFields (name, List expectedType, defaultValue) mem =
    case find (\(AstLabel n _) -> n == name) labeledFields of
        Just (AstLabel _ (AstList value)) ->
            if checkListType value expectedType mem
                then Right (AstLabel name (AstList value))
                else
                    Left
                        ( "Type mismatch for field '"
                            ++ name
                            ++ "', expected "
                            ++ show expectedType
                            ++ " but got \""
                            ++ show (AstList value)
                            ++ "\"; list isn't valid."
                        )
        _ -> Right (AstLabel name defaultValue)
validateField labeledFields (name, expectedType, defaultValue) mem =
    case find (\(AstLabel n _) -> n == name) labeledFields of
        Just (AstLabel _ value) ->
            if isValidType value expectedType
                then Right (AstLabel name value)
                else case exhaustTypes name expectedType value mem of
                    Right ast -> Right (AstLabel name ast)
                    Left err ->
                        Left ("Type mismatch for field '"
                            ++ name
                            ++ "', expected "
                            ++ show expectedType
                            ++ " but got ("
                            ++ err
                            ++ ").")
        _ -> Right (AstLabel name defaultValue)

mergeFields :: [(String, MarylType, Ast)] -> Either String [Ast] -> Memory -> Either String [Ast]
mergeFields defFields labeledFields mem =
    either
        Left
        (\labeledFields -> traverse (\field -> validateField labeledFields field mem) defFields)
        labeledFields

normalizeStruct :: Ast -> Ast -> Memory -> Either String Ast
normalizeStruct (AstDefineStruct (Structure _ structProps)) (AstStruct instanceFields) mem =
    let definedFields =
            map
                ( \(AstDefineVar (Variable name varType defaultValue)) ->
                    (name, varType, defaultValue)
                )
                structProps
        labeledFields = case instanceFields of
            (AstLabel _ _ : _) -> Right instanceFields
            _ -> matchPositionalFields definedFields instanceFields

        validatedFields = mergeFields definedFields labeledFields mem
     in case validatedFields of
        Right normalized -> evalFinalStruct normalized (AstStruct normalized)
        Left err -> Left err
normalizeStruct (AstDefineStruct (Structure _ structProps)) AstVoid mem =
    let definedFields =
            map
                ( \(AstDefineVar (Variable name varType defaultValue)) ->
                    (name, varType, defaultValue)
                )
                structProps
        defaultLabeledFields = Right (map (\(name, _, defaultValue) -> AstLabel name defaultValue) definedFields)

        validatedFields = mergeFields definedFields defaultLabeledFields mem
     in case validatedFields of
        Right normalized -> evalFinalStruct normalized (AstStruct normalized)
        Left err -> Left err
normalizeStruct _ _ _ = Left "Invalid struct definition or instance."

evalFinalStruct :: [Ast] -> Ast -> Either String Ast -- check if there are extra values
evalFinalStruct ((AstLabel label AstVoid) : _) _ =
    Left ("Defining a structure requires no uninitialised values, expected value for field '" ++ label ++ "'.")
evalFinalStruct ((AstLabel _ _) : xs) ast = evalFinalStruct xs ast
evalFinalStruct [] ast = Right ast
evalFinalStruct _ ast = Right ast
