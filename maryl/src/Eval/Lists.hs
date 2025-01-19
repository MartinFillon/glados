{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lists
-}

module Eval.Lists (checkIndices, checkListType, evalList, evalListElemDef, getAtIdx, getIndexes, updateList) where

import Data.List (intercalate, foldl')
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..), MarylType (..), Variable (..), getMarylType)

-- | Get list element based on a list of index.
getAtIdx :: Ast -> [Int] -> Either String Ast
getAtIdx (AstList elements) [idx]
    | idx >= 0 && idx < length elements = Right (elements !! idx)
    | otherwise = Left ("Index " ++ show idx ++ " out of bounds for list of size " ++ show (length elements) ++ ".")
getAtIdx (AstList elements) (x : xs)
    | x >= 0 && x < length elements =
        let current = elements !! x
         in getAtIdx current xs
    | otherwise = Left ("Index " ++ show x ++ " out of bounds for list of size " ++ show (length elements) ++ ".")
getAtIdx ast _ =
    Left ("Invalid operation: expected a list but got " ++ show ast ++ ".")

-- | Change a list element based on a list of index.
changeAtIdx :: Ast -> [Int] -> Ast -> Either String Ast
changeAtIdx (AstList elements) [idx] newVal
    | idx >= 0 && idx < length elements =
        Right (AstList (take idx elements ++ [newVal] ++ drop (idx + 1) elements))
    | otherwise =
        Left ("Index " ++ show idx ++ " out of bounds for list of size " ++ show (length elements) ++ ".")
changeAtIdx (AstList elements) (x : xs) newVal
    | x >= 0 && x < length elements =
        let current = elements !! x
         in case changeAtIdx current xs newVal of
                Right updated ->
                    Right (AstList (take x elements ++ [updated] ++ drop (x + 1) elements))
                Left err -> Left err
    | otherwise =
        Left ("Index " ++ show x ++ " out of bounds for list of size " ++ show (length elements) ++ ".")
changeAtIdx ast _ _ =
    Left (" expected a list but got " ++ show ast ++ ".")

getIndexFromAst' :: Memory -> (Memory -> Ast -> Maybe Int) -> String -> Maybe Int
getIndexFromAst' mem f n = case readMemory mem n of
    Just ast -> f mem ast
    Nothing -> Nothing

getIndexFromAst :: Memory -> Ast -> Maybe Int
getIndexFromAst _ (AstInt i) = Just i
getIndexFromAst mem (AstVar v) = getIndexFromAst' mem getIndexFromAst v
getIndexFromAst _ _ = Nothing

-- | Evaluates a list of AST types to a list of indexes.
getIndexes :: Memory -> [Ast] -> Either String [Int]
getIndexes mem = foldl' (\acc ast -> case getIndexFromAst mem ast of
    Just i -> accumulate acc i
    Nothing -> Left $ "Couldn't find index for " ++ show ast
    ) (Right [])
    where
        accumulate :: Either String [Int] -> Int -> Either String [Int]
        accumulate acc i = acc >>= \curr -> return $ curr ++ [i]

updateList :: String -> Ast -> Memory -> Ast -> Either String (Ast, Memory)
updateList listName (AstListElem _ idxs) mem newVal =
    case readMemory mem listName of
        Just (AstList elements) ->
            getIndexes mem idxs >>= \idxs' -> 
                case changeAtIdx (AstList elements) idxs' newVal of
                    Right updatedList -> Right (updatedList, mem)
                    Left err -> Left ("Error updating list: " ++ err)
        Just (AstString s) ->
            getIndexes mem idxs >>= \idxs' ->
                case changeAtIdx (AstList (convertStringToList (AstString s))) idxs' newVal of
                    Right updatedList -> case convertListToString updatedList of
                        Right newS -> Right (newS, mem)
                        Left err -> Left err 
                    Left err -> Left ("Error updating string: " ++ err)
        _ -> Left ("Unable to update " ++ show (AstListElem listName idxs) ++ " with " ++ show newVal ++ ".")
updateList _ ast mem _ = Right (ast, mem)

-- | Check validity of indexes based on the list (for calls at index).
checkIndices :: [Int] -> [Ast] -> Either String ()
checkIndices [] _ = Right ()
checkIndices (i : idxs) list
    | i < 0 || i >= length list =
        Left ("Index " ++ show i ++ " is out of bounds for the list.")
    | otherwise = case list !! i of
        AstList sublist -> checkIndices idxs sublist
        _ ->
            if null idxs
                then Right ()
                else Left ("Invalid indexing at depth, cannot be applied for [" ++ intercalate "][" (map show idxs) ++ "].")

-- | Checkouts a whole list with an expectedType to validate it.
checkListType :: [Ast] -> MarylType -> Memory -> Bool
checkListType (x : xs) (Struct typeStruct) mem = -- !!  TO DO fix this
    case readMemory mem typeStruct of
        Just (AstDefineStruct _) ->
            checkListType xs (Struct typeStruct) mem
        Just (AstGlobal (AstDefineStruct _)) ->
            checkListType xs (Struct typeStruct) mem
        _ -> False
checkListType ((AstList x) : xs) (List eleType) mem
    | checkListType x eleType mem = checkListType xs (List eleType) mem
    | otherwise = False
checkListType (AstVar var : xs) expectedType mem =
    maybe False (\val -> checkListType (val : xs) expectedType mem) (readMemory mem var)
checkListType ((AstListElem var idxs) : xs) expectedType mem =
    case readMemory mem var of
        Just (AstList eles) -> case getIndexes mem idxs of
            Right idxs' -> case getAtIdx (AstList eles) idxs' of
                Right _ -> checkListType xs expectedType mem
                _ -> False
            _ -> False
        Just (AstString s) -> case getIndexes mem (convertStringToList (AstString s)) of 
            Right idxs' -> case getAtIdx (AstList (convertStringToList (AstString s))) idxs' of
                Right _ -> checkListType xs expectedType mem
                _ -> False
            _ -> False
        _ -> False
checkListType (x : xs) expectedType mem
    | getMarylType x == expectedType = checkListType xs expectedType mem
    | otherwise = False
checkListType [] _ _ = True

-- | Evaluate a single list definition.
evalListElemDef :: String -> [Ast] -> MarylType -> Memory -> Either String Ast
evalListElemDef listVar idx typeVar mem =
    case readMemory mem listVar of
        Just (AstList eles) ->
            if getMarylType (head eles) == typeVar
                then Right (AstListElem listVar idx)
                else Left ("List element isn't of proper type, expected " ++ show typeVar ++ ".")
        Just (AstString _) ->
            if Char == typeVar
                then Right (AstListElem listVar idx)
                else Left "Elements within strings can only be characters."
        Just (AstArg (AstDefineVar (Variable _ String _)) _) ->
            if Char == typeVar
                then Right (AstListElem listVar idx)
                else Left "Elements within strings can only be characters."
        Just val -> Left ("Variable " ++ listVar ++ " isn't referencing to type List but " ++ show val ++ ".")
        Nothing -> Left ("Variable " ++ listVar ++ " out of scope.")

-- | Transform a string into a list of AstChar.
convertStringToList :: Ast -> [Ast]
convertStringToList (AstString s) = map AstChar s
convertStringToList _ = []

-- | Transform an AstList of AstChar into a string
convertListToString :: Ast -> Either String Ast
convertListToString (AstList astList) =
    let extractChar (AstChar c) = Right c
        extractChar _ = Left "List contains a non-AstChar element."
     in case traverse extractChar astList of
            Right chars -> Right $ AstString chars
            Left err -> Left err
convertListToString _ = Left "Invalid input, expecting a AstList."

-- | Evaluate index(es) call of a list.
evalList :: String -> [Ast] -> Memory -> Either String (Ast, Memory)
evalList var idxs mem = case readMemory mem var of
    Just (AstList list) -> getIndexes mem idxs
        >>= \idxs' -> case checkIndices idxs' list of
            Right () -> Right (AstListElem var idxs, mem)
            Left err -> Left (var ++ ": " ++ err)
    Just (AstString s) -> getIndexes mem idxs
        >>= \idxs' -> case checkIndices idxs' (convertStringToList (AstString s)) of
            Right () -> Right (AstListElem var idxs, mem)
            Left err -> Left (var ++ ": " ++ err)
    Just _ -> Left ("Index call of variable \"" ++ var ++ "\" isn't available; only supported by type list.")
    Nothing -> Left ("Variable \"" ++ var ++ "\" is out of scope; not defined.")
