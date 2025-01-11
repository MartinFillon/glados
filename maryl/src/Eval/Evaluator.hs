{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, applyOp, defaultRegistry) where

import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Assignment (updateList)
import Eval.Ops (evalAdd, evalAnd, evalBAnd, evalBOr, evalBXor, evalDiv, evalEq, evalGreaterThan, evalLessThan, evalMod, evalMul, evalNEq, evalOr, evalPower, evalShiftL, evalShiftR, evalSub)
import Memory (Memory, addMemory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Variable (..))

type FunctionRegistry =
    Map.Map String (Memory -> Ast -> Ast -> Either String (Ast, Memory))

evalAssign :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAssign mem (AstVar var) right =
    evalNode mem right >>= \(evaluatedR, updatedMem) ->
        let newMem = updateMemory updatedMem var evaluatedR
         in Right (evaluatedR, newMem)
evalAssign mem (AstListElem var (x : xs)) right =
    evalNode mem right >>= \(evaluatedAst, updatedMem) ->
        updateList var (AstListElem var (x : xs)) updatedMem evaluatedAst >>= \(clarified, newMem) ->
            let finalMem = updateMemory newMem var clarified
             in Right (evaluatedAst, finalMem)
evalAssign _ left right = Left ("Can't assign " ++ show right ++ " to " ++ show right ++ ".")

defaultRegistry :: FunctionRegistry
defaultRegistry =
    Map.fromList
        [ ("+", evalAdd),
          ("-", evalSub),
          ("*", evalMul),
          ("/", evalDiv),
          ("%", evalMod),
          ("**", evalPower),
          ("|", evalBOr),
          ("&", evalBAnd),
          (">>", evalShiftR),
          ("<<", evalShiftL),
          ("^", evalBXor),
          ("==", evalEq),
          ("!=", evalNEq),
          (">", evalGreaterThan),
          -- (">=", evalGreaterThan),
          ("<", evalLessThan),
          -- ("<=", evalLessThan),
          ("or", evalAnd),
          ("and", evalOr)
        ]

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

applyOp :: Memory -> String -> Ast -> Ast -> Either String (Ast, Memory)
applyOp mem op left right =
    maybeToEither
        (op ++ " is not a valid operator")
        (Map.lookup op defaultRegistry)
        >>= (\f -> f mem left right)

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem (AstVar name) =
    case readMemory mem name of
        Just value -> Right (value, mem)
        Nothing -> Left $ "Undefined variable: " ++ name
evalNode mem (AstDefineVar (Variable varName varType vVal)) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem ->
                Right
                    ( AstDefineVar (Variable varName varType vVal),
                      finalMem
                    )
            Left err -> Left $ "Failed to define var (" ++ err ++ ")"
evalNode mem (AstDefineFunc (Function funcName args body typ)) =
    let clearedMem = freeMemory mem
     in evalAST clearedMem body >>= \(evaluatedBody, updatedMem) ->
            let evaluatedFunction = Function funcName args evaluatedBody typ
             in case addMemory updatedMem funcName (AstDefineFunc evaluatedFunction) of
                    Right finalMem -> Right (AstVoid, finalMem)
                    Left err -> Left $ "Failed to define function (" ++ err ++ ")"
evalNode mem (AstBinaryFunc "=" left right) = evalAssign mem left right
evalNode mem (AstBinaryFunc op left right) = do
    (leftVal, mem') <- evalNode mem left
    (rightVal, mem'') <- evalNode mem' right
    applyOp mem'' op leftVal rightVal
evalNode mem (AstReturn expr) =
    evalNode mem expr >>= \(evaluatedExpr, mem') ->
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
evalNode mem (AstList elems) = do
    (evaluatedElems, mem') <- evalAST mem elems
    Right (AstList evaluatedElems, mem')
evalNode mem node = Right (node, mem)

-- Evaluate a list of AST nodes
evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) =
    case evalNode mem ast of
        Left err -> Left (show ast ++ ":\n\t |- " ++ err)
        Right (transformedAst, updatedMem) ->
            evalAST updatedMem asts >>= \(restAst, finalMem) ->
                Right (transformedAst : restAst, finalMem)

-- Helper to extract block contents
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []
