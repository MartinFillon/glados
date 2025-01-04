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
import Memory (Memory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Variable (..))

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
evalNode mem (AstDefineVar (Variable name _ val)) =
    trace (show val) $
        evalNode mem val >>= \(evaluatedExpr, updatedMem) ->
            Right (evaluatedExpr, updateMemory updatedMem name evaluatedExpr)
-- evalNode mem (AstDefineFunc (Function name args body typ))
evalNode _ rest = Left ("TODO: " ++ show rest)

evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) = trace ("evaluating " ++ show ast) $
    case evalNode mem ast of
        Left err -> Left err
        Right (transformedAst, updatedMem) -> trace ("mem: " ++ show updatedMem) $ do
            (restAst, finalMem) <- evalAST updatedMem asts
            return (transformedAst : restAst, finalMem)
