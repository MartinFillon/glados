{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode) where

import Debug.Trace (trace)
import Memory (Memory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
-- evalNode mem (AstVar name) =
--     case readMemory mem name of
--         Just value -> Right (value, mem)
--         Nothing -> Left $ "Undefined variable: " ++ name
evalNode mem (AstDefineVar (Variable varName varType vVal)) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        Right (AstDefineVar (Variable varName varType vVal), updateMemory updatedMem varName evaluatedExpr)
evalNode mem (AstDefineFunc (Function funcName args body typ)) = do
    let newMem = freeMemory mem
     in evalAST newMem body >>= \(evaluatedBody, updatedMem) ->
            let evaluatedFunction = Function funcName args evaluatedBody typ
             in Right (AstVoid, updateMemory updatedMem funcName (AstDefineFunc evaluatedFunction))
evalNode mem rest = Right (rest, mem)

evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) =
    case evalNode mem ast of
        Left err -> Left err
        Right (transformedAst, updatedMem) ->
            evalAST updatedMem asts >>= \(restAst, finalMem) ->
                Right (transformedAst : restAst, finalMem)
