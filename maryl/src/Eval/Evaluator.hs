{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Eval.Evaluator (evalAST) where

import Debug.Trace (trace)
import Memory (Memory, readMemory, updateMemory, freeMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem AstVoid = Right (AstVoid, mem)
evalNode mem (AstInt n) = Right (AstInt n, mem)
evalNode mem (AstBool b) = Right (AstBool b, mem)
evalNode mem (AstString s) = Right (AstString s, mem)
evalNode mem (AstChar c) = Right (AstChar c, mem)
evalNode mem (AstDouble d) = Right (AstDouble d, mem)
evalNode mem (AstReturn val) = evalNode mem val
evalNode mem (AstVar name) =
    case readMemory mem name of
        Just value -> Right (value, mem)
        Nothing -> Left $ "Undefined variable: " ++ name
evalNode mem (AstDefineVar (Variable vName vType vVal)) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        Right (AstDefineVar (Variable vName vType vVal), updateMemory updatedMem vName evaluatedExpr)
evalNode mem (AstDefineFunc (Function funcName args body typ)) = do
    let newMem = freeMemory mem
    (evaluatedBody, updatedMem) <- evalAST newMem body
    let evaluatedFunction = Function funcName args evaluatedBody typ
    Right (AstVoid, updateMemory updatedMem funcName (AstDefineFunc evaluatedFunction))
evalNode mem rest = Right (rest, mem)

evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) =
    -- trace ("evaluating " ++ show ast) $
    case evalNode mem ast of
        Left err -> Left err
        Right (transformedAst, updatedMem) -> 
            evalAST updatedMem asts >>= \(restAst, finalMem) ->
            Right (transformedAst : restAst, finalMem)
