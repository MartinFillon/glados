{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Eval.Evaluator (evalAST) where

import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..))
-- import Debug.Trace (trace)

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem (AstInt n) = Right (AstInt n, mem)
evalNode mem (AstBool b) = Right (AstBool b, mem)
evalNode mem (AstString s) = Right (AstString s, mem)
evalNode mem (AstVar name) =
    case readMemory mem name of
        Just value -> Right (value, mem)
        Nothing -> Left $ "Undefined variable: " ++ name
evalNode _ rest = Left ("TODO: " ++ show rest)

evalAST :: Memory -> [Ast] -> Either String (Ast, Memory)
evalAST mem [] = Right (AstVoid, mem)
evalAST mem (ast:asts) = 
    case evalNode mem ast of
        Left err -> Left err
        Right (_, newMem) ->
            evalAST newMem asts
