{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Assignment
-}

module Eval.Assignment (updateList, getIndexes) where

import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..))
import Data.List (foldl')

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
        Just (AstList elements) -> getIndexes mem idxs >>= \idxs' -> Right (changeAtIdx (AstList elements) idxs' newVal, mem)
        _ -> Left ("Unable to update " ++ show (AstListElem listName idxs) ++ " with " ++ show newVal ++ ".")
updateList _ ast mem _ = Right (ast, mem)
