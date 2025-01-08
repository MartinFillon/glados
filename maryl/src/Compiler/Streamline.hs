{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Streamline
-}

module Compiler.Streamline (clarifyAST, updateList) where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Eval.Evaluator (simplifyOp)
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..))

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

updateList :: String -> Ast -> Memory -> Ast -> (Ast, Memory)
updateList listName (AstListElem _ idxs) mem newVal =
    case readMemory mem listName of
        Just (AstList elements) -> (changeAtIdx (AstList elements) idxs newVal, mem)
        _ -> (AstVoid, mem)
updateList _ ast mem _ = (ast, mem)

-- simplifyOp :: String -> Ast -> Ast -> Memory -> Ast
-- simplifyOp "+" left right mem = clarifyAST left mem + clarifyAST right mem
-- simplifyOp "-" left right mem = clarifyAST left mem - clarifyAST right mem
-- simplifyOp "*" left right mem = clarifyAST left mem * clarifyAST right mem
-- simplifyOp "/" left right mem = clarifyAST left mem `div` clarifyAST right mem
-- simplifyOp "%" left right = clarifyAST left mem % clarifyAST right mem

clarifyAST :: Ast -> Memory -> Ast
clarifyAST (AstBinaryFunc op left right) mem =
    case simplifyOp mem op (clarifyAST left mem) (clarifyAST right mem) of
        Right (result, _) -> result
        _ -> AstVoid
clarifyAST (AstVar var) mem = fromMaybe AstVoid (readMemory mem var)
clarifyAST ast _ = ast

-- terner
-- operators
-- var
