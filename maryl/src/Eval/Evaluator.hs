{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, applyOp, defaultRegistry) where

import Compiler.Translation.Functions (isBuiltin)
import Data.List (intercalate)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Assignment (updateList)
import Eval.Ops (evalAdd, evalAnd, evalBAnd, evalBOr, evalBXor, evalDiv, evalEq, evalGreatThanEq, evalGreaterThan, evalLessThan, evalLessThanEq, evalMod, evalMul, evalNEq, evalOr, evalPower, evalShiftL, evalShiftR, evalSub)
import Memory (Memory, addMemory, freeMemory, generateUniqueLoopName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Structure (..), Variable (..), getMarylType, isValidType)

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
    evalNode mem right >>= \(evaluatedAst, updatedMem) ->
        updateList var (AstListElem var idxs) updatedMem evaluatedAst >>= \(clarified, newMem) ->
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
          ("==", evalEq),
          ("!=", evalNEq),
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

evalDefinition :: Ast -> MarylType -> Variable -> Memory -> Either String Ast
evalDefinition AstVoid _ _ _ = Right AstVoid
evalDefinition (AstArg ast _) typeVar var mem = evalDefinition ast typeVar var mem
evalDefinition (AstVar str) typeVar var mem = case readMemory mem str of
    Just value -> evalDefinition value typeVar var mem
    Nothing -> Left ("Variable " ++ str ++ " out of scope.")
evalDefinition (AstListElem listVar idx) typeVar var mem =
    case readMemory mem listVar of
        Just (AstList eles) ->
            if getMarylType (head eles) == typeVar
                then Right (AstListElem listVar idx)
                else Left ("List element isn't of proper type, expected " ++ show typeVar ++ ".")
        Just _ -> Left ("Variable " ++ listVar ++ " isn't referencing to type List.")
        Nothing -> Left ("Variable " ++ listVar ++ " out of scope.")
evalDefinition (AstList eles) (List typeVar) var mem
    | getMarylType (head eles) == typeVar = Right (AstList eles)
    | otherwise = Left ("List element isn't of proper type, expected " ++ show typeVar ++ ".")
evalDefinition (AstDefineVar origVar@(Variable varName varType _)) expectedType var mem
    | varType == expectedType = Right (AstDefineVar origVar)
    | otherwise = Left (varName ++ " isn't of proper type, expected " ++ show varType ++ ".")
evalDefinition ast (Struct structType) var@(Variable varName _ _) mem =
    case readMemory mem structType of
        Just (AstDefineStruct struct) -> Right ast
        _ -> Left ("Struct of type " ++ structType ++ " can't be found, " ++ varName ++ " can't be defined.")
evalDefinition ast varType var@(Variable varName _ varValue) mem
    | isValidType ast varType = Right ast
    | otherwise = Left ("Value " ++ varName ++ " isn't typed correctly, expected " ++ show varType ++ ".")

evalStructDecla :: [Ast] -> Memory -> Either String ()
-- evalStructDecla ((AstDefineVar var@(Variable varName varType AstVoid)) : xs) mem =
--     Left ("Defining a structure requires no uninitialised values, expected " ++ show varType ++ " for " ++ varName)
evalStructDecla ((AstDefineVar var@(Variable varName varType varValue)) : xs) mem =
    case evalDefinition varValue varType var mem of
        -- Right AstVoid -> Left ("Defining a structure requires no uninitialised values, expected " ++ show varType ++ " for " ++ varName)
        Right _ -> evalStructDecla xs mem
        Left err -> Left err
evalStructDecla [] mem = Right ()

evalArgs :: [Ast] -> Memory -> Memory
evalArgs [] mem = mem
evalArgs ((AstDefineVar (Variable varName varType varValue)) : xs) mem =
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

checkListType :: [Ast] -> MarylType -> Memory -> Bool
checkListType (x : xs) (List typeList) mem
    | getMarylType x == typeList = checkListType xs typeList mem
    | otherwise = False
checkListType (AstVar var : xs) (List typeList) mem =
    case readMemory mem var of
        Just val -> checkListType (val : xs) typeList mem
        _ -> False

-- handle struct

checkIndices :: [Int] -> [Ast] -> Either String ()
checkIndices [] _ = Right ()
checkIndices (i : idxs) list
    | i < 0 || i >= length list = Left ("Index " ++ show i ++ " is out of bounds for the list.")
    | otherwise = case list !! i of
        AstList sublist -> checkIndices idxs sublist
        _ ->
            if null idxs
                then Right ()
                else Left ("Invalid indexing at depth, cannot be applied for " ++ show list ++ "[" ++ intercalate "][" (map show idxs) ++ "].")

evalList :: String -> [Int] -> Memory -> Either String (Ast, Memory)
evalList var idxs mem = case readMemory mem var of
    Just (AstList list) ->
        case checkIndices idxs list of
            Right () -> Right (AstListElem var idxs, mem)
            Left err -> Left err
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
evalNode mem (AstDefineVar var@(Variable varName varType varValue))
    | isValidType varValue varType = addDefineVar mem var
    | otherwise = case evalDefinition varValue varType var mem of
        Right _ -> addDefineVar mem var
        Left err -> Left err
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
evalNode mem (AstListElem var idxs) = evalList var idxs mem -- >> checkListType
evalNode mem (AstDefineStruct struct@(Structure name properties)) =
    case addMemory mem name (AstDefineStruct struct) of
        Right newMem -> case evalStructDecla properties newMem of
            Right () -> Right (AstDefineStruct struct, newMem)
            Left err -> Left err
        Left err -> Left ("Failed to define structure (" ++ err ++ ").")
-- translateAST (AstStruct eles) mem =
-- >> return AstDefineStruct that updates values
evalNode mem (AstReturn expr) =
    evalNode mem expr >>= \(evaluatedExpr, mem') ->
        Right (AstReturn evaluatedExpr, mem')
evalNode mem node = Right (node, mem)

-- Evaluate a list of AST nodes
evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem [] = Right ([], mem)
evalAST mem (ast : asts) = trace ("[[ " ++ show ast ++ " ]]") $
    case evalNode mem ast of
        Left err -> Left (show ast ++ ":\n\t |- " ++ err)
        Right (transformedAst, updatedMem) ->
            evalAST updatedMem asts >>= \(restAst, finalMem) ->
                Right (transformedAst : restAst, finalMem)

-- Helper to extract block contents
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []
