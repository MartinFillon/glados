{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Streamline
-}

module Compiler.Streamline (clarifyAST) where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Eval.Evaluator (applyOp)
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..))

clarifyAST :: Ast -> Memory -> Ast
clarifyAST (AstBinaryFunc op left right) mem =
    case applyOp mem op (clarifyAST left mem) (clarifyAST right mem) of
        Right (result, _) -> result
        _ -> AstVoid
clarifyAST (AstVar var) mem = fromMaybe AstVoid (readMemory mem var)
-- clarifyAST (AstFunc (Function funcName funcArgs _ _)) mem =
clarifyAST ast _ = ast

-- terner
