{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, simplifyOp) where

import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Ops (evalAdd, evalDiv, evalMod, evalMul, evalSub)
import Memory (Memory, addMemory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))

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
-- evalNode mem (AstVar name) =
--     case readMemory mem name of
--         Just value -> Right (value, mem)
--         Nothing -> Left $ "Undefined variable: " ++ name
evalNode mem (AstDefineVar (Variable varName varType vVal)) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem -> Right
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
evalNode mem rest = Right (rest, mem)

evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) = trace (show (ast : asts)) $
    case evalNode mem ast of
        Left err -> Left err
        Right (transformedAst, updatedMem) ->
            evalAST updatedMem asts >>= \(restAst, finalMem) ->
                Right (transformedAst : restAst, finalMem)
