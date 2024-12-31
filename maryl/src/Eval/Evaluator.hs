{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Eval.Evaluator (evalAST) where

import Memory (Memory)
import Parsing.ParserAst (Ast (..))
import Debug.Trace (trace)

evalAST :: Memory -> [Ast] -> Either String (Ast, Memory)
evalAST mem ast = trace (show mem ++ "  " ++ show ast) $
    Left "Invalid AST1"
