{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Assignment
-}

module Eval.Structures (evalFinalStruct, normalizeStruct) where

import Data.List (find)
import Eval.Lists (checkListType, getAtIdx, getIndexes)
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..), MarylType (..), Structure (..), Variable (..), getMarylType, isValidType)

{- | Match the definition of fields within a structure based on position, with no label as so:
>>> struct vector s = {1, 2, 5}
-}
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

-- | Extend the check of to justify type in a structure's field.
exhaustTypes :: String -> MarylType -> Ast -> Memory -> Either String Ast
exhaustTypes _ expectedType (AstListElem var idxs) mem =
    case readMemory mem var of
        Just (AstList eles) ->
            getIndexes mem idxs >>= \idxs' -> case getAtIdx (AstList eles) idxs' of
                Right ast ->
                    if isValidType ast expectedType
                        then Right ast
                        else Left (show ast)
                Left err -> Left err
        _ -> Left "invalid list call."
exhaustTypes name expectedType (AstArg ast _) mem = exhaustTypes name expectedType ast mem
exhaustTypes name expectedType (AstVar var) mem =
    maybe (Left var) (\val -> exhaustTypes name expectedType val mem) (readMemory mem var) 
exhaustTypes _ expectedType ast _
    | isValidType ast expectedType = Right ast
    | otherwise = Left (show ast)

-- | Validate a single structure's field and its value.
validateField :: [Ast] -> (String, MarylType, Ast) -> Memory -> Either String Ast
validateField labeledFields (name, fieldType, defaultValue) mem =
    case findField labeledFields name of
        Just (AstLabel _ value) -> validateValue name fieldType value mem
        Nothing -> Right (AstLabel name defaultValue)

-- | Helper to find a labeled field by name
findField :: [Ast] -> String -> Maybe Ast
findField fields fieldName = find (\(AstLabel n _) -> n == fieldName) fields

-- | Validate the value of a field against its expected type
validateValue :: String -> MarylType -> Ast -> Memory -> Either String Ast -- !! Add for struct
validateValue name (List expectedType) (AstList value) mem
    | checkListType value expectedType mem = Right (AstLabel name (AstList value))
    | otherwise =
        Left ("Type mismatch for field '" ++ name ++ "', expected "
            ++ show expectedType ++ " but got an invalid list.")
validateValue name (List expectedType) (AstVar var) mem =
    maybe (Left (var ++ " doesn't exits.")) (\val ->
        validateValue name (List expectedType) val mem) (readMemory mem var)
validateValue name expectedType value mem
    | isValidType value expectedType = Right (AstLabel name value)
    | otherwise = case exhaustTypes name expectedType value mem of
        Right ast -> Right (AstLabel name ast)
        Left err ->
            Left ("Type mismatch for field '" ++ name ++ "', expected "
                ++ show expectedType ++ " but got (" ++ err ++ ").")

-- | Truncate initialised structure with its newly defined fields.
mergeFields :: [(String, MarylType, Ast)] -> Either String [Ast] -> Memory -> Either String [Ast]
mergeFields defFields (Right labeledFields) mem =
    let definedFieldNames = map (\(name, _, _) -> name) defFields
        labelFieldNames = map (\(AstLabel name _) -> name) labeledFields
        extraFields = filter (`notElem` definedFieldNames) labelFieldNames
     in if not (null extraFields)
           then Left ("Unexpected fields in structure: " ++ show extraFields ++ ".")
           else traverse (\field -> validateField labeledFields field mem) defFields
mergeFields _ (Left err) _ = Left err

-- | Take a defined struct type and normalise based on a struct declaration.
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

-- | Evaluate the final normalized struct, ensuring all labels are valid and no extra fields are present.
evalFinalStruct :: [Ast] -> Ast -> Either String Ast
evalFinalStruct ((AstLabel label AstVoid) : _) _ =
    Left ("Defining a structure requires no uninitialised values, expected value for field '" ++ label ++ "'.")
evalFinalStruct ((AstLabel _ _) : xs) ast = evalFinalStruct xs ast
evalFinalStruct [] ast = Right ast
evalFinalStruct _ ast = Right ast
