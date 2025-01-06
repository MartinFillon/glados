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

changeAtIdx :: Ast -> Int -> Ast -> Ast
changeAtIdx (AstList elements) idx newVal | idx >= 0 && idx < length elements =
        AstList (take idx elements ++ [newVal] ++ drop (idx + 1) elements)
    | otherwise = error "Index out of bounds"
changeAtIdx _ _ _ = error "Index out of bounds"

updateList :: String -> Ast -> Memory -> Ast -> (Ast, Memory)
updateList listName (AstListElem _ (idx: _)) mem newVal =
    case readMemory mem listName of
        Just val -> (changeAtIdx val (fromIntegral idx) newVal, mem)
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
