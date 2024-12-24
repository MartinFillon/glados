{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}

module Eval.Evaluator (evalAST) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Memory (Memory, readMemory, updateMemory)
import Parsing.ParserAst (Ast(..), Function(..))

type FunctionRegistry =
    Map.Map String (Memory -> [Ast] -> Either String (Ast, Memory))

defaultRegistry :: FunctionRegistry
defaultRegistry =
    Map.fromList
        [ ("+", evalBinaryOp AstInt (+)),
          ("-", evalBinaryOp AstInt (-)),
          ("*", evalBinaryOp AstInt (*)),
          ("/", evalBinaryOp AstInt div),
          ("%", evalBinaryOp AstInt mod)
        ]

-- Helper function to evaluate binary operators
evalBinaryOp :: (Integer -> Ast) -> (Integer -> Integer -> Integer) -> Memory -> [Ast] -> Either String (Ast, Memory)
evalBinaryOp constructor op mem [AstInt x, AstInt y] =
    Right (constructor (x `op` y), mem)
evalBinaryOp _ _ _ _ = Left "Invalid arguments for binary operator"

evalAST :: Memory -> Ast -> Either String (Ast, Memory)
evalAST mem (AstInt i) = Right (AstInt i, mem)
evalAST mem (AstBool b) = Right (AstBool b, mem)
evalAST mem (AstString s) = Right (AstString s, mem)
evalAST mem (AstChar c) = Right (AstChar c, mem)
evalAST mem AstVoid = Right (AstVoid, mem)
evalAST mem (AstVar name) =
    maybe (Left $ "Undefined variable: " ++ name) (\val -> Right (val, mem)) (readMemory mem name)
evalAST mem (AstBinaryFunc op left right) = do
    (evaluatedLeft, mem') <- evalAST mem left
    (evaluatedRight, mem'') <- evalAST mem' right
    case Map.lookup op defaultRegistry of
        Just func -> func mem'' [evaluatedLeft, evaluatedRight]
        Nothing -> Left $ "Unknown operator: " ++ op

evalAST mem (AstDefineVar var) =
    let name = vName var
        value = vValue var
    in evalAST mem value >>= \(evaluatedValue, updatedMem) ->
           Right (AstVoid, updateMemory updatedMem name evaluatedValue)

evalAST mem (AstIf cond thenBranch elseIfs maybeElse) = do
    (AstBool condResult, mem') <- evalAST mem cond
    if condResult
        then evalAST mem' thenBranch
        else evalElseIfs mem' elseIfs maybeElse
  where
    evalElseIfs :: Memory -> [Ast] -> Maybe Ast -> Either String (Ast, Memory)
    evalElseIfs mem [] (Just elseBranch) = evalAST mem elseBranch
    evalElseIfs mem [] Nothing = Right (AstVoid, mem)
    evalElseIfs mem (AstIf cond body _ _ : rest) maybeElse = do
        (AstBool condResult, mem') <- evalAST mem cond
        if condResult
            then evalAST mem' body
            else evalElseIfs mem' rest maybeElse
    evalElseIfs _ _ _ = Left "Malformed else-if structure"

evalAST mem (AstReturn expr) = evalAST mem expr

evalAST _ _ = Left "Unhandled AST node"
