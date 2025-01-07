{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Streamline
-}

module Compiler.Streamline (clarifyAST, updateList) where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))

changeAtIdx :: Ast -> [Integer] -> Ast -> Ast
changeAtIdx (AstList elements) [idx] newVal
    | fromIntegral idx >= 0 && fromIntegral idx < length elements =
        AstList (take (fromIntegral idx) elements ++ [newVal] ++ drop (fromIntegral idx + 1) elements)
    | otherwise = AstList elements
changeAtIdx (AstList elements) (x : xs) newVal
    | fromIntegral x >= 0 && fromIntegral x < length elements =
        let current = elements !! fromIntegral x
            updated = changeAtIdx current xs newVal
         in AstList (take (fromIntegral x) elements ++ [updated] ++ drop (fromIntegral x + 1) elements)
    | otherwise = AstList elements
changeAtIdx ast _ _ = ast

updateList :: String -> Ast -> Memory -> Ast -> (Ast, Memory)
updateList listName (AstListElem _ idxs) mem newVal =
    case readMemory mem listName of
        Just (AstList elements) -> (changeAtIdx (AstList elements) idxs newVal, mem)
        _ -> (AstVoid, mem)
updateList _ ast mem _ = (ast, mem)

-- simplifyOp :: String -> Ast -> Ast -> Ast
-- simplifyOp "+" left right = clarifyAST left mem + clarifyAST right mem
-- simplifyOp "-" left right = clarifyAST left mem - clarifyAST right mem
-- simplifyOp "*" left right = clarifyAST left mem * clarifyAST right mem
-- simplifyOp "/" left right = clarifyAST left mem `div` clarifyAST right mem
-- simplifyOp "%" left right = clarifyAST left mem % clarifyAST right mem

clarifyAST :: Ast -> Memory -> Ast
-- clarifyAst (AstBinaryFunc op left right) mem = simplifyOp op left right
clarifyAST (AstVar var) mem = fromMaybe AstVoid (readMemory mem var)
clarifyAST ast mem = ast

-- terner
-- operators
-- var
