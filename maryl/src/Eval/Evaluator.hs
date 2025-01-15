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
evalAssign mem (AstListElem var idxs) right = trace ("list elem: " ++ show var ++ " " ++ show idxs)$
    evalNode mem right >>= \(evaluatedAst, updatedMem) -> trace ("-->" ++ show var ++ show idxs ++ " - " ++ show evaluatedAst) $
        updateList var (AstListElem var idxs) updatedMem evaluatedAst >>= \(clarified, newMem) -> trace ("!! " ++ show clarified) $
            let finalMem = updateMemory newMem var clarified
             in Right (AstBinaryFunc "=" (AstVar var) clarified, finalMem)
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
        Left err -> Left ("Operation failed with" ++ show right ++ " invalid for " ++ op ++ " (" ++ err ++ ").")
    Left err -> Left ("Operation failed with" ++ show left ++ " invalid for " ++ op ++ " (" ++ err ++ ").")

----- Declarations (Functions, Variables, Loop)

evalArgs :: [Ast] -> Memory -> Memory
evalArgs [] mem = mem
evalArgs ((AstDefineVar (Variable varName varType varValue)): xs) mem =
    evalArgs xs (updateMemory mem varName (AstArg (AstDefineVar (Variable varName varType varValue)) Nothing))
evalArgs _ mem = mem

checkBuiltins :: String -> Ast -> Memory -> Either String (Ast, Memory)
checkBuiltins func ast mem
    | isBuiltin func = Right (ast, mem) -- add some error handling?
    | otherwise = Left ("Function \"" ++ show func ++ " isn't defined.")

addDefineFunc :: Memory -> Function -> Either String (Ast, Memory)
addDefineFunc mem func@(Function funcName args body typ) =
    case addMemory mem funcName (AstDefineFunc func) of
        Right newMem ->
            let editedMem = evalArgs args (freeMemory newMem)
             in evalAST editedMem body >>= \(evaluatedBody, updatedMem) ->
                    let updatedFunc = func {fBody = evaluatedBody}
                     in Right (AstVoid, updateMemory updatedMem funcName (AstDefineFunc updatedFunc))
        Left err -> Left $ "Failed to define function (" ++ err ++ ")."

addDefineLoopFunc :: String -> Ast -> Ast -> Memory -> Memory
addDefineLoopFunc loopName cond block mem =
    case addMemory mem loopName (AstDefineLoop loopName cond block) of
        Right newMem -> newMem
        _ -> mem

addDefineVar :: Memory -> Variable -> Either String (Ast, Memory)
addDefineVar mem var@(Variable varName _ vVal) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem ->
                Right (AstDefineVar var, finalMem)
            Left err -> Left $ "Failed to define var (" ++ err ++ ")."

----- Types

evalList :: String -> [Int] -> Memory -> Either String (Ast, Memory)
evalList var idxs mem = case readMemory mem var of
    Just (AstList _) -> trace ("----- giving " ++ show var ++ " " ++ show idxs) $ Right (AstListElem var idxs, mem)
    Just _ -> Left ("Index call of variable \"" ++ show var ++ "\" isn't available; only supported by type list.")
    Nothing -> Left ("Variable \"" ++ show var ++ "\" is out of scope; not defined.")

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
    maybe (Left $ "Undefined variable: " ++ name ++ ".") (\value -> Right (value, mem)) (readMemory mem name)
evalNode mem (AstDefineVar var) = addDefineVar mem var
evalNode mem (AstDefineFunc func) = addDefineFunc mem func
evalNode mem (AstFunc func@(Function funcName _ _ _)) =
    case readMemory mem funcName of
        Just (AstDefineFunc (Function _ _ _ newFuncType)) -> Right (AstFunc (func {fType = newFuncType}), mem)
        _ -> checkBuiltins funcName (AstFunc func) mem
evalNode mem (AstLoop Nothing cond block) =
    let loopName = generateUniqueLoopName mem
        updatedMem = addDefineLoopFunc loopName cond block mem
     in Right (AstLoop (Just loopName) cond block, updatedMem)
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
evalNode mem (AstListElem var idxs) = evalList var idxs mem
evalNode mem (AstReturn expr) =
    evalNode mem expr >>= \(evaluatedExpr, mem') ->
        Right (AstReturn evaluatedExpr, mem')
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
