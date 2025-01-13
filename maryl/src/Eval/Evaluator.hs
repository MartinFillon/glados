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
import Eval.Ops (evalAdd, evalAnd, evalBAnd, evalBOr, evalBXor, evalDiv, evalEq, evalGreatThanEq, evalGreaterThan, evalLessThan, evalLessThanEq, evalMod, evalMul, evalNEq, evalOr, evalPower, evalShiftL, evalShiftR, evalSub)
import Memory (Memory, addMemory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))

type FunctionRegistry =
    Map.Map String (Memory -> Ast -> Ast -> Either String (Ast, Memory))

----- Operators

-- (=)
evalAssign :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAssign mem (AstVar var) right =
    evalNode mem right >>= \(evaluatedR, updatedMem) ->
        let newMem = updateMemory updatedMem var evaluatedR
         in Right (AstBinaryFunc "=" (AstVar var) evaluatedR, newMem)
evalAssign mem (AstListElem var (x : xs)) right =
    evalNode mem right >>= \(evaluatedAst, updatedMem) ->
        updateList var (AstListElem var (x : xs)) updatedMem evaluatedAst >>= \(clarified, newMem) ->
            let finalMem = updateMemory newMem var clarified
             in Right (AstVoid, finalMem)
evalAssign _ left right = Left ("Can't assign " ++ show right ++ " to " ++ show left ++ ".")

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
          --   ("==", evalEq),
          --   ("!=", evalNEq),
          (">", evalGreaterThan),
          (">=", evalGreatThanEq),
          ("<", evalLessThan),
          ("<=", evalLessThanEq),
          ("or", evalOr),
          ("and", evalAnd)
        ]

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

applyOp :: Memory -> String -> Ast -> Ast -> Either String (Ast, Memory)
applyOp mem op left right =
    maybeToEither
        (op ++ " is not a valid operator")
        (Map.lookup op defaultRegistry)
        >>= (\f -> f mem left right)

-----

evalList :: String -> [Int] -> Memory -> Either String (Ast, Memory)
evalList var idxs mem = case readMemory mem var of
    Just (AstList elems) -> Right (AstListElem var idxs, mem)
    Just val -> Left ("Index call of variable \"" ++ show var ++ "\" isn't available; only supported by type list.")
    Nothing -> Left ("Variable \"" ++ show var ++ "\" is out of scope; not defined.")
evalList ast _ _ = Left ("Invalid index call with \"" ++ show ast ++ "\", expected a variable.")

-----

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem (AstBinaryFunc "=" left right) = evalAssign mem left right
evalNode mem (AstPrefixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstPostfixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstBinaryFunc (x : "=") left right)
    | x /= '=' && x /= '!' = evalAssign mem left (AstBinaryFunc [x] left right)
    | otherwise = evalNode mem (AstBinaryFunc (x : "=") left right)
evalNode mem (AstBinaryFunc op left right) = do
    (leftVal, mem') <- evalNode mem left
    (rightVal, mem'') <- evalNode mem' right
    applyOp mem'' op leftVal rightVal
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
evalNode mem (AstReturn expr) =
    evalNode mem expr >>= \(evaluatedExpr, mem') ->
        Right (AstReturn evaluatedExpr, mem')
-- evalNode mem (AstIf cond trueBranch elseIfBranches elseBranch) = do
--     (condResult, mem') <- evalNode mem cond
--     case condResult of
--         AstBool True -> do
--             (evaluatedBlock, mem'') <- evalAST mem' (extractBlock trueBranch)
--             Right (AstBlock evaluatedBlock, mem'')
--         AstBool False ->
--             case elseIfBranches of
--                 (AstIf elifCond elifTrue [] Nothing : rest) ->
--                     evalNode mem' (AstIf elifCond elifTrue rest elseBranch)
--                 [] -> case elseBranch of
--                     Just block -> do
--                         (evaluatedBlock, mem'') <- evalAST mem' (extractBlock block)
--                         Right (AstBlock evaluatedBlock, mem'')
--                     Nothing -> Right (AstVoid, mem')
--                 _ -> Left "Invalid else-if structure"
--         _ -> Left "Condition in if statement is not a boolean"
-- evalNode mem (AstLoop cond body) = do
--     let loop mem' = do
--             (condResult, mem'') <- evalNode mem' cond
--             case condResult of
--                 AstBool True -> do
--                     (_, mem''') <- evalAST mem'' (extractBlock body)
--                     loop mem'''
--                 AstBool False -> Right (AstVoid, mem'')
--                 _ -> Left "Condition in loop is not a boolean"
--     loop mem
evalNode mem (AstListElem var idxs) = evalList var idxs mem
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
