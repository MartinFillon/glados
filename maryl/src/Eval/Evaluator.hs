{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, applyOp, defaultRegistry) where

import Compiler.Translation.Functions (isBuiltin)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Assignment (updateList)
import Eval.Ops (evalAdd, evalAnd, evalBAnd, evalBOr, evalBXor, evalDiv, evalEq, evalGreatThanEq, evalGreaterThan, evalLessThan, evalLessThanEq, evalMod, evalMul, evalNEq, evalOr, evalPower, evalShiftL, evalShiftR, evalSub)
import Memory (Memory, addMemory, freeMemory, generateUniqueLoopName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Variable (..))

type FunctionRegistry =
    Map.Map String (Memory -> Ast -> Ast -> Either String (Ast, Memory))

----- Operators

-- (=)
evalAssign :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAssign mem (AstVar var) right =
    evalNode mem right >>= \(evaluatedR, updatedMem) ->
        let newMem = updateMemory updatedMem var evaluatedR
         in Right (AstBinaryFunc "=" (AstVar var) evaluatedR, newMem)
evalAssign mem (AstListElem var idxs) right =
    evalNode mem right >>= \(evaluatedAst, updatedMem) -> trace ("-->" ++ show var ++ show idxs ++ " - " ++ show evaluatedAst) $
        updateList var (AstListElem var idxs) updatedMem evaluatedAst >>= \(clarified, newMem) -> trace (show clarified) $
            let finalMem = updateMemory newMem var clarified
             in Right (AstVar var, finalMem)
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

evalBinaryFunc :: Memory -> String -> Ast -> Ast -> Either String (Ast, Memory)
evalBinaryFunc mem op left right = case evalNode mem left of
    Right (leftVal, mem') -> case evalNode mem' right of
        Right (rightVal, mem'') -> applyOp mem'' op leftVal rightVal
        Left err -> Left err
        _ -> Left ("Argument " ++ show right ++ " invalid for operation " ++ op ++ ".")
    Left err -> Left err
    _ -> Left ("Argument " ++ show left ++ " invalid for operation " ++ op ++ ".")

-----

addLoopFunction :: String -> Ast -> Ast -> Memory -> Either String Memory
addLoopFunction loopName cond block mem =
    addMemory mem loopName (AstDefineLoop loopName cond block)

updatedArgs :: [Ast] -> Memory -> Memory
updatedArgs ((AstDefineVar (Variable varName varType varValue)): xs) mem =
    updatedArgs xs (updateMemory mem varName (AstArg (AstDefineVar (Variable varName varType varValue)) Nothing))
updatedArgs [] mem = mem

checkBuiltins :: String -> Ast -> Memory -> Either String (Ast, Memory)
checkBuiltins func ast mem
    | isBuiltin func = Right (ast, mem)
    | otherwise = Left ("Function \"" ++ show func ++ " isn't defined.")

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
evalNode mem (AstBinaryFunc (x : "=") left right)
    | x /= '=' && x /= '!' = evalAssign mem left (AstBinaryFunc [x] left right)
    | otherwise = evalNode mem (AstBinaryFunc (x : "=") left right)
evalNode mem (AstBinaryFunc op left right) = evalBinaryFunc mem op left right
evalNode mem (AstPrefixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstPostfixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstVar name) =
    case readMemory mem name of
        Just value -> Right (value, mem)
        Nothing -> Left $ "Undefined variable: " ++ name ++ "."
evalNode mem (AstDefineVar (Variable varName varType vVal)) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem ->
                Right
                    ( AstDefineVar (Variable varName varType vVal),
                      finalMem
                    )
            Left err -> Left $ "Failed to define var (" ++ err ++ ")."
evalNode mem (AstDefineFunc (Function funcName args body typ)) =
    case addMemory mem funcName (AstDefineFunc (Function funcName args body typ)) of
        Right newMem ->
            let editedMem = updatedArgs args (freeMemory newMem)
             in evalAST editedMem body >>= \(evaluatedBody, updatedMem) ->
                    let evaluatedFunction = Function funcName args evaluatedBody typ
                     in Right (AstVoid, updateMemory updatedMem funcName (AstDefineFunc evaluatedFunction))
        Left err -> Left $ "Failed to define function (" ++ err ++ ")."
evalNode mem (AstFunc (Function funcName funcArgs funcBody funcType)) =
    case readMemory mem funcName of
        Just (AstDefineFunc (Function _ _ _ newFuncType)) -> Right (AstFunc (Function funcName funcArgs funcBody newFuncType), mem)
        _ -> checkBuiltins funcName (AstFunc (Function funcName funcArgs funcBody funcType)) mem
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
evalNode mem (AstLoop Nothing cond block) =
    let loopName = generateUniqueLoopName mem
     in case addLoopFunction loopName cond block mem of
        Right updatedMem -> Right (AstLoop (Just (AstString loopName)) cond block, updatedMem)
evalNode mem (AstListElem var idxs) = evalList var idxs mem
evalNode mem node = Right (node, mem)

-- Evaluate a list of AST nodes
evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) = trace ("[[ "++ show ast ++" ]]")$
    case evalNode mem ast of
        Left err -> Left (show ast ++ ":\n\t |- " ++ err)
        Right (transformedAst, updatedMem) ->
            evalAST updatedMem asts >>= \(restAst, finalMem) ->
                Right (transformedAst : restAst, finalMem)

-- Helper to extract block contents
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []
