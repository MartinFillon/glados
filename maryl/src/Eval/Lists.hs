{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lists
-}

module Eval.Lists (checkIndices, checkListType, evalList, evalListElemDef, getIndexes, updateList) where

import Data.List (intercalate, foldl')
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..), MarylType (..), getMarylType)

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

getIndexFromAst' :: Memory -> (Memory -> Ast -> Maybe Int) -> String -> Maybe Int
getIndexFromAst' mem f n = case readMemory mem n of
    Just ast -> f mem ast
    Nothing -> Nothing

getIndexFromAst :: Memory -> Ast -> Maybe Int
getIndexFromAst _ (AstInt i) = Just i
getIndexFromAst mem (AstVar v) = getIndexFromAst' mem getIndexFromAst v
getIndexFromAst _ _ = Nothing

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
            getIndexes mem idxs >>= \idxs' -> Right (changeAtIdx (AstList elements) idxs' newVal, mem)
        _ -> Left ("Unable to update " ++ show (AstListElem listName idxs) ++ " with " ++ show newVal ++ ".")
updateList _ ast mem _ = Right (ast, mem)

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

checkListType :: [Ast] -> MarylType -> Memory -> Bool
checkListType (x : xs) (Struct typeStruct) mem = -- fix this
    case readMemory mem typeStruct of
        Just (AstDefineStruct _) -> checkListType xs (Struct typeStruct) mem
        _ -> False
checkListType ((AstList x) : xs) (List eleType) mem
    | checkListType x eleType mem = checkListType xs (List eleType) mem
    | otherwise = False
checkListType (AstVar var : xs) expectedType mem =
    maybe False (\val -> checkListType (val : xs) expectedType mem) (readMemory mem var)
checkListType (x : xs) expectedType mem
    | getMarylType x == expectedType = checkListType xs expectedType mem
    | otherwise = False
checkListType [] _ _ = True
-- handle list elem

evalListElemDef :: String -> [Ast] -> MarylType -> Memory -> Either String Ast
evalListElemDef listVar idx typeVar mem =
    case readMemory mem listVar of
        Just (AstList eles) ->
            if getMarylType (head eles) == typeVar
                then Right (AstListElem listVar idx)
                else Left ("List element isn't of proper type, expected " ++ show typeVar ++ ".")
        Just _ -> Left ("Variable " ++ listVar ++ " isn't referencing to type List.")
        Nothing -> Left ("Variable " ++ listVar ++ " out of scope.")

evalList :: String -> [Ast] -> Memory -> Either String (Ast, Memory)
evalList var idxs mem = case readMemory mem var of
    Just (AstList list) -> getIndexes mem idxs
        >>= \idxs' -> case checkIndices idxs' list of
            Right () -> Right (AstListElem var idxs, mem)
            Left err -> Left (var ++ ": " ++ err)
    Just _ -> Left ("Index call of variable \"" ++ var ++ "\" isn't available; only supported by type list.")
    Nothing -> Left ("Variable \"" ++ var ++ "\" is out of scope; not defined.")
