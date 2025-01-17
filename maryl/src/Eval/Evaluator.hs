{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, applyOp, defaultRegistry) where

import qualified Data.DList as D
import Data.List (intercalate)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Functions (checkBuiltins, evalArgs)
import Eval.Lists (checkListType, evalList, evalListElemDef, updateList)
import Eval.Ops (
    evalAdd,
    evalAnd,
    evalBAnd,
    evalBOr,
    evalBXor,
    evalDiv,
    evalEq,
    evalGreatThanEq,
    evalGreaterThan,
    evalLessThan,
    evalLessThanEq,
    evalMod,
    evalMul,
    evalNEq,
    evalOr,
    evalPower,
    evalShiftL,
    evalShiftR,
    evalSub
    )
import Eval.Structures (normalizeStruct)
import Memory (Memory, addMemory, freeMemory, generateUniqueLoopName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Structure (..), Variable (..), getMarylType, isSameType, isValidType)

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
             in Right (AstBinaryFunc "=" (AstListElem var idxs) right, finalMem)
evalAssign _ left right = Left ("Can't assign " ++ show right ++ " to " ++ show left ++ ".")
-- struct
-- astarg

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

----- Declarations (Defined Variables/ Defined Structures)

evalDefinition :: Ast -> MarylType -> Variable -> Memory -> Either String Ast
evalDefinition AstVoid _ _ _ = Right AstVoid
evalDefinition (AstArg ast _) typeVar var mem = evalDefinition ast typeVar var mem
evalDefinition (AstVar str) typeVar var mem = case readMemory mem str of
    Just value -> evalDefinition value typeVar var mem
    Nothing -> Left ("Variable " ++ str ++ " out of scope.")
evalDefinition (AstListElem listVar idx) typeVar _ mem = evalListElemDef listVar idx typeVar mem
evalDefinition (AstList eles) (List typeVar) _ mem
    | checkListType eles typeVar mem = Right (AstList eles)
    | otherwise = Left ("Element in list isn't of proper type, expected list full of " ++ show typeVar ++ ".")
evalDefinition (AstDefineVar origVar@(Variable varName varType _)) expectedType _ _
    | varType == expectedType = Right (AstDefineVar origVar)
    | otherwise = Left (varName ++ " isn't of proper type, expected " ++ show varType ++ ".")
evalDefinition ast (Struct structType) (Variable varName _ _) mem =
    case readMemory mem structType of
        Just (AstDefineStruct struct) -> normalizeStruct (AstDefineStruct struct) ast
        _ -> Left ("Struct of type " ++ structType ++ " can't be found, " ++ varName ++ " can't be defined.")
evalDefinition (AstFunc funcName) _ _ _ =
    trace "to do: evaluate functions " Right (AstFunc funcName)
evalDefinition (AstTernary _ doState elseState) expectedType var mem =
    -- eval condition?
    case evalDefinition doState expectedType var mem of
        Right _ -> evalDefinition elseState expectedType var mem
        _ -> Left (show doState ++ " in ternary isn't typed correctly, expected " ++ show expectedType ++ ".")
evalDefinition ast varType (Variable varName _ _) _
    | isValidType ast varType = Right ast
    | otherwise = Left ("Value " ++ varName ++ " isn't typed correctly, expected " ++ show varType ++ ".")

evalStructDecla :: [Ast] -> Memory -> Either String ()
evalStructDecla ((AstDefineVar var@(Variable _ varType varValue)) : xs) mem =
    case evalDefinition varValue varType var mem of
        Right _ -> evalStructDecla xs mem
        Left err -> Left err
evalStructDecla [] _ = Right ()
evalStructDecla _ _ = Left "Invalid definition of structure."

----- Memory-based definitions

addDefineFunc :: Memory -> Function -> Either String (Ast, Memory)
addDefineFunc mem func@(Function funcName args body _) =
    case addMemory mem funcName (AstDefineFunc func) of
        Right newMem ->
            let editedMem = evalArgs args (freeMemory newMem)
             in evalAST editedMem body >>= \(evaluatedBody, updatedMem) ->
                    let updatedFunc = func {fBody = evaluatedBody}
                     in Right (AstVoid, updateMemory updatedMem funcName (AstDefineFunc updatedFunc))
        Left err -> Left $ "Failed to define function (" ++ err ++ ")."

addLoopLabel :: String -> Ast -> Ast -> Memory -> Memory
addLoopLabel loopName cond block mem =
    case addMemory mem loopName (AstGlobal (AstLoop (Just loopName) cond block)) of
        Right newMem -> newMem
        _ -> mem

addDefineVar :: Memory -> Variable -> Either String (Ast, Memory)
addDefineVar mem var@(Variable varName _ vVal) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem ->
                Right (AstDefineVar var, finalMem)
            Left err -> Left $ "Failed to define var (" ++ err ++ ")."

-----

boolTokens :: [Char]
boolTokens = ['=', '!', '<', '>']

evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem (AstBinaryFunc "=" left right) = evalAssign mem left right
evalNode mem (AstBinaryFunc (x : "=") left right)
    | x `notElem` boolTokens = evalAssign mem left (AstBinaryFunc [x] left right)
    | otherwise = evalBinaryFunc mem (x : "=") left right
evalNode mem (AstBinaryFunc op left right) = evalBinaryFunc mem op left right
evalNode mem (AstPrefixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstPostfixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstVar name) =
    maybe (Left $ "Undefined variable: " ++ name ++ ".") (\value -> Right (value, mem)) (readMemory mem name)
evalNode mem (AstDefineVar var@(Variable _ varType varValue))
    | isValidType varValue varType = addDefineVar mem var
    | otherwise =
        evalDefinition varValue varType var mem >> addDefineVar mem var
evalNode mem (AstDefineFunc func) = addDefineFunc mem func
evalNode mem (AstFunc func@(Function funcName _ _ _)) =
    case readMemory mem funcName of
        Just (AstDefineFunc (Function _ _ _ newFuncType)) -> Right (AstFunc (func {fType = newFuncType}), mem)
        _ -> checkBuiltins funcName (AstFunc func) mem
evalNode mem (AstLoop Nothing cond block) =
    let loopName = generateUniqueLoopName mem
        updatedMem = addLoopLabel loopName cond block mem
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
evalNode mem (AstIf cond trueBranch elseIfBranches elseBranch) =
    evalNode mem cond >>= \(condResult, mem') -> case condResult of
        AstTernary c t e -> evalNode mem' (AstIf c t [] (Just e)) >> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem')
        AstVar var -> case readMemory mem' var of
            Just (AstBool b) -> evalNode mem' (AstIf (AstBool b) trueBranch elseIfBranches elseBranch) >> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem')
            _ -> Left $ "Unknown variable \"" ++ var ++ "\""
        AstFunc func -> case readMemory mem' (fName func) of
            Just (AstDefineFunc f) ->
                if fType f == Bool
                    then evalAST mem' (extractBlock trueBranch) >>= \(_, mem'') -> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem'')
                    else Left $ "Function \"" ++ fName f ++ "\" is not a function of type bool"
            _ -> Left $ "Unknown function \"" ++ fName func ++ "\""
        AstBool True ->
            evalAST mem' (extractBlock trueBranch)
                >>= \(_, mem'') -> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem'')
        AstBool False ->
            case elseIfBranches of
                (AstIf elifCond elifTrue [] Nothing : rest) ->
                    evalNode mem' (AstIf elifCond elifTrue rest elseBranch) >> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem')
                [] -> case elseBranch of
                    Just block ->
                        evalAST mem' (extractBlock block)
                            >>= \(_, mem'') -> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem'')
                    Nothing -> Right (AstIf cond trueBranch elseIfBranches elseBranch, mem')
                _ -> Left "Invalid else-if structure"
        _ -> Left "Condition in if statement is not a boolean"
evalNode mem (AstTernary cond trueBranch elseBranch) =
    if isSameType trueBranch elseBranch
        then evalNode mem (AstIf cond trueBranch [] (Just elseBranch)) >> Right (AstTernary cond trueBranch elseBranch, mem)
        else Left "Mismatching types in ternary operation"
evalNode mem (AstListElem var idxs) = evalList var idxs mem
evalNode mem (AstDefineStruct struct@(Structure name properties)) =
    case addMemory mem name (AstDefineStruct struct) of
        Right newMem ->
            evalStructDecla properties newMem >>= \() ->
                Right (AstDefineStruct struct, newMem)
        Left err -> Left ("Failed to define structure (" ++ err ++ ").")
evalNode mem (AstReturn expr) =
    evalNode mem expr >>= \(evaluatedExpr, mem') ->
        Right (AstReturn expr, mem')
evalNode mem node = Right (node, mem)

-- Evaluate a list of AST nodes
evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem asts = evalAST' mem asts D.empty

evalAST' :: Memory -> [Ast] -> D.DList Ast -> Either String ([Ast], Memory)
evalAST' mem [] acc = Right (D.toList acc, mem)
evalAST' mem (ast : asts) acc =
    trace ("[[ " ++ show ast ++ " ]]") $
        case evalNode mem ast of
            Left err -> Left (show ast ++ ":\n\t |- " ++ err)
            Right (transformedAst, updatedMem) ->
                evalAST' updatedMem asts (acc `D.snoc` transformedAst)

-- Helper to extract block contents
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []
