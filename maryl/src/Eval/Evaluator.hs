{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, simplifyOp) where

import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Ops (evalAdd, evalDiv, evalMod, evalMul, evalSub)
import Memory (Memory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Variable (..))

type FunctionRegistry =
    Map.Map String (Memory -> Ast -> Ast -> Either String (Ast, Memory))

defaultRegistry :: FunctionRegistry
defaultRegistry =
    Map.fromList
        [ ("+", evalAdd),
          ("-", evalSub),
          ("*", evalMul),
          ("/", evalDiv),
          ("%", evalMod)
          --   ("and", evalAnd),
          --   ("or", evalOr)
        ]

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

simplifyOp :: Memory -> String -> Ast -> Ast -> Either String (Ast, Memory)
simplifyOp m n left right =
    maybeToEither
        "Not a valid operator"
        (Map.lookup n defaultRegistry)
        >>= (\f -> f m left right)

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem (AstVar name) =
  case readMemory mem name of
    Just value -> Right (value, mem)
    Nothing -> Left $ "Undefined variable: " ++ name
evalNode mem (AstDefineVar (Variable varName varType vVal)) =
  evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
    Right
      ( AstDefineVar (Variable varName varType evaluatedExpr),
        updateMemory updatedMem varName evaluatedExpr
      )
evalNode mem (AstDefineFunc (Function funcName args body typ)) = do
  let newMem = freeMemory mem
  evalAST newMem body >>= \(evaluatedBody, updatedMem) ->
    let evaluatedFunction = Function funcName args evaluatedBody typ
     in Right
          (AstVoid, updateMemory updatedMem funcName (AstDefineFunc evaluatedFunction))
evalNode mem (AstBinaryFunc op left right) = do
  (leftVal, mem') <- evalNode mem left
  (rightVal, mem'') <- evalNode mem' right
  evalBinary op leftVal rightVal mem''
evalNode mem (AstReturn expr) = do
  (evaluatedExpr, mem') <- evalNode mem expr
  Right (AstReturn evaluatedExpr, mem')
evalNode mem (AstIf cond trueBranch elseIfBranches elseBranch) = do
  (condResult, mem') <- evalNode mem cond
  case condResult of
    AstBool True -> do
      (evaluatedBlock, mem'') <- evalAST mem' (extractBlock trueBranch)
      Right (AstBlock evaluatedBlock, mem'')
    AstBool False ->
      case elseIfBranches of
        (AstIf elifCond elifTrue [] Nothing : rest) ->
          evalNode mem' (AstIf elifCond elifTrue rest elseBranch)
        [] -> case elseBranch of
          Just block -> do
            (evaluatedBlock, mem'') <- evalAST mem' (extractBlock block)
            Right (AstBlock evaluatedBlock, mem'')
          Nothing -> Right (AstVoid, mem')
        _ -> Left "Invalid else-if structure"
    _ -> Left "Condition in if statement is not a boolean"
evalNode mem (AstLoop cond body) = do
  let loop mem' = do
        (condResult, mem'') <- evalNode mem' cond
        case condResult of
          AstBool True -> do
            (_, mem''') <- evalAST mem'' (extractBlock body)
            loop mem'''
          AstBool False -> Right (AstVoid, mem'')
          _ -> Left "Condition in loop is not a boolean"
  loop mem
evalNode mem AstBreak = Right (AstBreak, mem)
evalNode mem AstVoid = Right (AstVoid, mem)
evalNode mem (AstList elems) = do
  (evaluatedElems, mem') <- evalAST mem elems
  Right (AstList evaluatedElems, mem')
evalNode mem node = Right (node, mem)

-- Evaluate a binary operation
evalBinary :: String -> Ast -> Ast -> Memory -> Either String (Ast, Memory)
evalBinary "+" (AstInt l) (AstInt r) mem = Right (AstInt (l + r), mem)
evalBinary "-" (AstInt l) (AstInt r) mem = Right (AstInt (l - r), mem)
evalBinary "*" (AstInt l) (AstInt r) mem = Right (AstInt (l * r), mem)
evalBinary "/" (AstInt l) (AstInt r) mem =
  if r == 0
    then Left "Division by zero"
    else Right (AstInt (l `div` r), mem)
evalBinary op _ _ _ = Left $ "Unsupported binary operator: " ++ op

-- Evaluate a list of AST nodes
evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) = case evalNode mem ast of
    Left err -> Left err
    Right (transformedAst, updatedMem) ->
      evalAST updatedMem asts >>= \(restAst, finalMem) ->
        Right (transformedAst : restAst, finalMem)

-- Helper to extract block contents
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []

-- evalAST mem (ast : asts) = trace (show (ast : asts)) $
--     case evalNode mem ast of
--         Left err -> Left err
--         Right (transformedAst, updatedMem) ->
--             evalAST updatedMem asts >>= \(restAst, finalMem) ->
--                 Right (transformedAst : restAst, finalMem)
