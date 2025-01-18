{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode, applyOp, defaultRegistry) where

import qualified Data.DList as D
import Data.Either (fromRight)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Functions (checkBuiltins, evalArgs)
import Eval.Lists (checkListType, evalList, evalListElemDef, getIndexes, updateList)
import Eval.Ops (
    boolTokens,
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

evalAssignType :: Ast -> Ast -> Memory -> Either String (Ast, Memory) 
evalAssignType (AstDefineVar var@(Variable varName varType varValue)) right mem =
    either
        (\err -> Left (varName ++ " can't be reassigned: " ++ err))
        (\_ -> Right (AstVar varName, mem)) (evalDefinition right varType mem)
evalAssignType (AstArg ast _) right mem =
    evalNode mem ast >>= \(evaluatedR, updatedMem) -> evalAssignType ast right mem
evalAssignType ast right mem
    | getMarylType ast == Undefined = Left ("Can't assign " ++ show ast) --handle here
    | otherwise =
        either Left (\_ -> Right (ast, mem)) (evalDefinition right (getMarylType ast) mem)
evalAssignType _ _ mem = Right (AstVoid, mem)

-- (=)
evalAssign :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAssign mem (AstVar var) right = evalNode mem right >>= \(evaluatedR, updatedMem) ->
    let newMem = updateMemory updatedMem var evaluatedR
     in maybe (Left ("Failed to assign \"" ++ var ++ "\", variable is out of scope."))
        (\val -> evalAssignType val evaluatedR mem >> Right (AstBinaryFunc "=" (AstVar var) evaluatedR, newMem))
        (readMemory mem var)
evalAssign mem (AstListElem var idxs) right =
    evalNode mem right >>= \(evaluatedAst, updatedMem) ->
        updateList var (AstListElem var idxs) updatedMem evaluatedAst >>= \(clarified, newMem) ->
            let finalMem = updateMemory newMem var clarified
             in Right (AstBinaryFunc "=" (AstListElem var idxs) right, finalMem)
evalAssign _ left right = Left ("Can't assign " ++ show right ++ " to " ++ show left ++ ".")
-- struct

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

----- Condition-based (If/ Ternary/ Loops)

evalIfConditions :: Ast -> Ast -> Memory -> Either String (Ast, Memory)
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstTernary c t e) mem =
    evalNode mem (AstIf c t [] (Just e)) >> Right (AstIf cond doBlock elseifs elseBlock, mem)
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstVar var) mem =
    case readMemory mem var of
        Just (AstBool b) -> evalNode mem (AstIf (AstBool b) doBlock elseifs elseBlock) >>
            Right (AstIf cond doBlock elseifs elseBlock, mem)
        _ -> Left ("Unknown variable \"" ++ var ++ "\"")
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstFunc func) mem =
    case readMemory mem (fName func) of
        Just (AstDefineFunc f) ->
            if fType f == Bool
                then evalAST mem (extractBlock doBlock) >>= \(_, mem') ->
                    Right (AstIf cond doBlock elseifs elseBlock, mem')
                else Left ("Function \"" ++ fName f ++ "\" is not a function of type bool")
        _ -> Left ("Unknown function \"" ++ fName func ++ "\"")
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstBool True) mem =
    evalAST mem (extractBlock doBlock)
        >>= \(_, mem') -> Right (AstIf cond doBlock elseifs elseBlock, mem')
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstBool False) mem =
    case elseifs of
        (AstIf elifCond elifTrue [] Nothing : rest) ->
            evalNode mem (AstIf elifCond elifTrue rest elseBlock) >>
                Right (AstIf cond doBlock elseifs elseBlock, mem)
        [] -> case elseBlock of
            Just block ->
                evalAST mem (extractBlock block)
                    >>= \(_, mem') -> Right (AstIf cond doBlock elseifs elseBlock, mem')
            Nothing -> Right (AstIf cond doBlock elseifs elseBlock, mem)
        _ -> Left "Invalid else-if structure"
evalIfConditions _ _ _ = Left "Condition in if statement is not a boolean"

evalLoopNode :: String -> Ast -> Memory -> Either String Ast
evalLoopNode loopName (AstBreak _) _ = Right (AstBreak (Just loopName))
evalLoopNode loopName (AstContinue _) _ = Right (AstContinue (Just loopName))
evalLoopNode _ node _ = Right node

evalLoopBlock :: String -> [Ast] -> Memory -> Either String [Ast]
evalLoopBlock loopName [] _ = Right []
evalLoopBlock loopName (x:xs) mem =
    case evalLoopNode loopName x mem of
        Right updatedNode -> case evalLoopBlock loopName xs mem of
            Right updatedRest -> Right (updatedNode : updatedRest)
            Left err -> Left err
        Left err -> Left err

evalLoops :: Ast -> Memory -> Either String (Ast, Memory)
evalLoops (AstLoop (Just loopName) cond block) mem =
    case evalLoopBlock loopName (extractBlock block) mem of
        Right newBlock -> Right (AstLoop (Just loopName) cond (AstBlock newBlock), mem)
        Left err -> Left err
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

----- Declarations (Defined Variables/ Defined Structures)

evalDefinition :: Ast -> MarylType -> Memory -> Either String Ast
evalDefinition AstVoid _ _ = Right AstVoid
evalDefinition (AstArg ast _) typeVar mem = evalDefinition ast typeVar mem
evalDefinition (AstVar str) typeVar mem = case readMemory mem str of
    Just value -> evalDefinition value typeVar mem
    Nothing -> Left ("Variable " ++ str ++ " out of scope.")
evalDefinition (AstListElem listVar idx) typeVar mem = evalListElemDef listVar idx typeVar mem
evalDefinition (AstList eles) (List typeVar) mem
    | checkListType eles typeVar mem = Right (AstList eles)
    | otherwise = Left ("Element in list isn't of proper type, expected list full of " ++ show typeVar ++ ".")
evalDefinition (AstDefineVar origVar@(Variable varName varType _)) expectedType _
    | varType == expectedType = Right (AstDefineVar origVar)
    | otherwise = Left (varName ++ " isn't of proper type, expected " ++ show varType ++ ".")
evalDefinition ast (Struct structType) mem =
    case readMemory mem structType of
        Just (AstDefineStruct struct) -> normalizeStruct (AstDefineStruct struct) ast
        _ -> Left ("Struct of type " ++ structType ++ " can't be found.")
evalDefinition (AstFunc funcName) _ _ =
    trace "to do: evaluate functions " Right (AstFunc funcName)
evalDefinition (AstTernary _ doState elseState) expectedType mem =
    -- eval condition?
    case evalDefinition doState expectedType mem of
        Right _ -> evalDefinition elseState expectedType mem
        _ -> Left (show doState ++ " in ternary isn't typed correctly, expected " ++ show expectedType ++ ".")
evalDefinition ast varType _
    | isValidType ast varType = Right ast
    | otherwise =
        Left ("Value isn't typed correctly, expected " ++ show varType ++ ".")

evalStructDecla :: [Ast] -> Memory -> Either String ()
evalStructDecla ((AstDefineVar var@(Variable _ varType varValue)) : xs) mem =
    either Left (const (evalStructDecla xs mem)) (evalDefinition varValue varType mem)
evalStructDecla [] _ = Right ()
evalStructDecla _ _ = Left "Invalid definition of structure."

----- Memory-based definitions

addDefineFunc :: Memory -> Function -> Either String (Ast, Memory)
addDefineFunc mem func@(Function funcName args body _) =
    either (\err -> Left ("Failed to define function (" ++ err ++ ").")) (\newMem ->
        let editedMem = evalArgs args (freeMemory newMem)
             in evalAST editedMem body >>= \(evaluatedBody, mem') ->
                    let func' = func {fBody = evaluatedBody}
                     in Right (AstVoid, updateMemory mem' funcName (AstDefineFunc func'))
    ) (addMemory mem funcName (AstDefineFunc func))

addLoopLabel :: String -> Ast -> Ast -> Memory -> Memory
addLoopLabel loopName cond block mem =
    fromRight mem (addMemory mem loopName (AstGlobal (AstLoop (Just loopName) cond block)))

addDefineVar :: Memory -> Variable -> Either String (Ast, Memory)
addDefineVar mem var@(Variable varName _ vVal) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem ->
                Right (AstDefineVar var, finalMem)
            Left err -> Left ("Failed to define variable (" ++ err ++ ").")

-----

-- | Evaluate a single AST node.
evalNode :: Memory -> Ast -> Either String (Ast, Memory)
evalNode mem (AstBinaryFunc "=" left right) = evalAssign mem left right
evalNode mem (AstBinaryFunc (x : "=") left right)
    | x `notElem` boolTokens = evalAssign mem left (AstBinaryFunc [x] left right)
    | otherwise = evalBinaryFunc mem (x : "=") left right
evalNode mem (AstBinaryFunc op left right) = evalBinaryFunc mem op left right
evalNode mem (AstPrefixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstPostfixFunc (_ : xs) ast) = evalAssign mem ast (AstBinaryFunc xs ast (AstInt 1))
evalNode mem (AstVar name) =
    maybe (Left ("Undefined variable: " ++ name ++ ".")) (\value ->
        Right (value, mem)) (readMemory mem name)
evalNode mem (AstDefineVar var@(Variable varName varType varValue))
    | isValidType varValue varType = addDefineVar mem var
    | otherwise = either (\err -> Left (varName ++ " can't be defined: " ++ err))
        (\_ -> addDefineVar mem var) (evalDefinition varValue varType mem)
evalNode mem (AstDefineFunc func) = addDefineFunc mem func
evalNode mem (AstFunc func@(Function funcName _ _ _)) =
    case readMemory mem funcName of
        Just (AstDefineFunc (Function _ _ _ newFuncType)) ->
            Right (AstFunc (func {fType = newFuncType}), mem)
        _ -> checkBuiltins funcName (AstFunc func) mem
evalNode mem (AstLoop Nothing cond block) =
    let loopName = generateUniqueLoopName mem
     in evalLoops (AstLoop (Just loopName) cond block) (addLoopLabel loopName cond block mem)
evalNode mem (AstIf cond trueBranch elseIfBranches elseBranch) =
    evalNode mem cond >>= uncurry
        (evalIfConditions (AstIf cond trueBranch elseIfBranches elseBranch))
evalNode mem (AstTernary cond left right) =
    evalNode mem (AstIf cond left [] (Just right)) >> Right (AstTernary cond left right, mem)
evalNode mem (AstListElem var idxs) = evalList var idxs mem
evalNode mem (AstDefineStruct struct@(Structure name properties)) =
    either (\err -> Left ("Failed to define structure (" ++ err ++ ").")) (\newMem ->
        evalStructDecla properties newMem >>= \() ->
            Right (AstDefineStruct struct, newMem)
    ) (addMemory mem name (AstDefineStruct struct))
evalNode mem (AstReturn expr) = evalNode mem expr >>= \(evaluatedExpr, mem') ->
    Right (AstReturn expr, mem')
evalNode mem node = Right (node, mem)

-- | Evaluate a list of AST nodes.
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

-- | Helper to extract block contents.
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []
