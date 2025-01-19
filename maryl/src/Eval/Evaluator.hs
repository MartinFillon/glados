{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator for Maryl AST
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval.Evaluator (evalAST, evalNode) where

import Data.Bifunctor (first)
import qualified Data.DList as D
import Data.Either (fromRight)
import Debug.Trace (trace)
import Eval.Functions (checkBuiltins, evalArgs)
import Eval.Lists (checkListType, evalList, evalListElemDef, updateList)
import Eval.Ops (applyOp, boolTokens, evalBinaryRet, evalOpExpr)
import Eval.Structures (evalFinalStruct, normalizeStruct)
import Memory (Memory, addMemory, freeMemory, generateUniqueLoopName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Structure (..), Variable (..), getMarylType, isValidType)

----- Operators

-- | Evaluate reassignment of variable to justify redefinition.
evalAssignType :: Ast -> Ast -> Memory -> Either String (Ast, Memory)
evalAssignType (AstConst ast) _ _ = Left (show ast ++ " is a const, therefore not mutable.")
evalAssignType (AstGlobal ast) _ _ = Left ("Global value " ++ show ast ++ " is const, can't be redefined.")
evalAssignType (AstDefineVar (Variable varName varType _)) right mem =
    either
        (\err -> Left (varName ++ " can't be reassigned: " ++ err))
        (\_ -> Right (AstVar varName, mem))
        (evalDefinition right varType mem)
evalAssignType (AstArg ast _) right mem =
    evalNode mem right >>= uncurry (evalAssignType ast)
evalAssignType (AstStruct _) right mem =
    evalNode mem right >>= \(evaluatedR, updatedMem) -> case evaluatedR of
        (AstStruct newEles) -> case evalFinalStruct newEles (AstStruct newEles) of
            Right _ -> Right (AstStruct newEles, updatedMem)
            Left err -> Left err
        _ -> Left ("Can't assign " ++ show evaluatedR ++ " as struct.")
evalAssignType (AstBinaryFunc op left right) rightExpr mem =
    evalOpExpr op >>= \expectedTypes ->
        either
            Left
            (\_ -> Right (AstBinaryFunc op left right, mem))
            (evalMultTypeDef rightExpr expectedTypes mem)
evalAssignType ast right mem
    | getMarylType ast == Undefined =
        Left ("Can't assign " ++ show ast ++ ", type isn't recognised")
    | otherwise =
        either Left (\_ -> Right (ast, mem)) (evalDefinition right (getMarylType ast) mem)

-- | Evaluate multiple possible return types (for expressions)
evalMultTypeDef :: Ast -> [MarylType] -> Memory -> Either String Ast
evalMultTypeDef right [x] mem = either Left Right (evalDefinition right x mem)
evalMultTypeDef right (x : xs) mem =
    either (\_ -> evalMultTypeDef right xs mem) Right (evalDefinition right x mem)
evalMultTypeDef _ [] _ = Left "No type expected."

-- | Evaluate '=' operation, handling of assignment
evalAssign :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAssign mem (AstVar var) right =
    evalNode mem right >>= \(evaluatedR, updatedMem) ->
        let newMem = updateMemory updatedMem var evaluatedR
         in maybe
                (Left ("Failed to assign \"" ++ var ++ "\", variable is out of scope."))
                ( \val -> case evalAssignType val evaluatedR newMem of
                    Right _ -> Right (AstBinaryFunc "=" (AstVar var) right, newMem)
                    Left err -> Left err
                )
                (readMemory mem var)
evalAssign mem (AstListElem var idxs) right =
    evalNode mem right >>= \(evaluatedAst, updatedMem) ->
        updateList var (AstListElem var idxs) updatedMem evaluatedAst >>= \(clarified, newMem) ->
            let finalMem = updateMemory newMem var clarified
             in Right (AstBinaryFunc "=" (AstListElem var idxs) right, finalMem)
evalAssign _ left right = Left ("Can't assign " ++ show right ++ " to " ++ show left ++ ".")

-- struct

-- | Evaluate binary functions (of two inputs)
evalBinaryFunc :: Memory -> String -> Ast -> Ast -> Either String (Ast, Memory)
evalBinaryFunc mem op left right = case evalNode mem left of
    Right (leftVal, mem') -> case evalNode mem' right of
        Right (rightVal, mem'') -> applyOp mem'' op leftVal rightVal
        Left err -> Left ("Operation fail, " ++ show right ++ " invalid for " ++ op ++ " (" ++ err ++ ").")
    Left err -> Left err

----- Condition-based (If/ Ternary/ Loops)

-- | Evaluate if-type condition statements/ Ternary
evalIfConditions :: Ast -> Ast -> Memory -> Either String (Ast, Memory)
evalIfConditions _ AstTernary {} _ =
    Left "Ternary can't be defined within a statement."
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstVar var) mem =
    case readMemory mem var of
        Just (AstBool b) ->
            evalNode mem (AstIf (AstBool b) doBlock elseifs elseBlock)
                >> Right (AstIf cond doBlock elseifs elseBlock, mem)
        _ -> Left ("Unknown variable \"" ++ var ++ "\", no bool found.")
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstFunc func) mem =
    case readMemory mem (fName func) of
        Just (AstDefineFunc f) ->
            if fType f == Bool
                then
                    evalASTBlock mem (extractBlock doBlock) >>= \(_, mem') ->
                        Right (AstIf cond doBlock elseifs elseBlock, mem')
                else Left ("Function \"" ++ fName f ++ "\" is not a function of type bool.")
        _ -> Left ("Unknown call to function \"" ++ fName func ++ "\".")
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstBool True) mem =
    evalASTBlock mem (extractBlock doBlock)
        >>= \(_, mem') -> Right (AstIf cond doBlock elseifs elseBlock, mem')
evalIfConditions (AstIf cond doBlock elseifs elseBlock) (AstBool False) mem =
    case elseifs of
        (AstIf elifCond elifTrue [] Nothing : rest) ->
            evalNode mem (AstIf elifCond elifTrue rest elseBlock)
                >> Right (AstIf cond doBlock elseifs elseBlock, mem)
        [] -> case elseBlock of
            Just block ->
                evalASTBlock mem (extractBlock block)
                    >>= \(_, mem') -> Right (AstIf cond doBlock elseifs elseBlock, mem')
            Nothing -> Right (AstIf cond doBlock elseifs elseBlock, mem)
        _ -> Left "Invalid else-if structure."
evalIfConditions _ _ _ = Left "Condition is not a boolean."

-- | Evaluate else-if branches within blocks of loop.
evalLoopElseIf :: String -> [Ast] -> Memory -> Either String [Ast]
evalLoopElseIf _ [] _ = Right []
evalLoopElseIf loopName (AstIf elifCond elifTrue restElseIfs Nothing : rest) mem =
    evalLoopBlock loopName (extractBlock elifTrue) mem >>= \(updatedElifTrue, memAfterElif) ->
        case evalLoopElseIf loopName rest memAfterElif of
            Right restElseIfs' ->
                Right (AstIf elifCond (AstBlock updatedElifTrue) restElseIfs Nothing : restElseIfs')
            Left err -> Left err
evalLoopElseIf _ (ast : _) _ =
    Left ("Unexpected AST node in else-if branch: " ++ show ast)

-- | Evaluate else branch within blocks of loop.
evalLoopElse :: String -> Maybe Ast -> Memory -> Either String (Maybe Ast)
evalLoopElse loopName Nothing _ = Right Nothing
evalLoopElse loopName (Just branch) mem =
    case evalLoopBlock loopName (extractBlock branch) mem of
        Right (updatedElseBranch, _) ->
            Right (Just (AstBlock updatedElseBranch))
        Left err -> Left err

-- | Evaluate a single line within the 'do' block.
evalLoopNode :: String -> Ast -> Memory -> Either String (Ast, Memory)
evalLoopNode loopName (AstBreak _) mem = Right (AstBreak (Just loopName), mem)
evalLoopNode loopName (AstContinue _) mem = Right (AstContinue (Just loopName), mem)
evalLoopNode loopName (AstIf cond trueBranch elseIfBranches elseBranch) mem =
    evalNode mem (AstIf cond trueBranch elseIfBranches elseBranch) >>= \(_, updatedMem) ->
        evalLoopBlock loopName (extractBlock trueBranch) updatedMem >>= \(updatedTrueBranch, memAfterTrue) ->
            evalLoopElseIf loopName elseIfBranches memAfterTrue >>= \updatedElseIfs ->
                evalLoopElse loopName elseBranch memAfterTrue >>= \updatedElseBranch ->
                    Right (AstIf cond (AstBlock updatedTrueBranch) updatedElseIfs updatedElseBranch, memAfterTrue)
evalLoopNode _ node mem = evalNode mem node

-- | Evaluate the 'do' block in while condition.
evalLoopBlock :: String -> [Ast] -> Memory -> Either String ([Ast], Memory)
evalLoopBlock _ [] mem = Right ([], mem)
evalLoopBlock loopName asts mem =
    let evalBlock :: [Ast] -> D.DList Ast -> Memory -> Either String (D.DList Ast, Memory)
        evalBlock [] acc currentMem = Right (acc, currentMem)
        evalBlock (x : xs) acc currentMem =
            evalLoopNode loopName x currentMem >>= \(updatedNode, updatedMem) ->
                evalBlock xs (acc `D.snoc` updatedNode) updatedMem
     in fmap (first D.toList) (evalBlock asts D.empty mem)

-- | Evaluate the condition of while loop.
evalLoopCond :: Ast -> Memory -> Either String (Ast, Memory)
evalLoopCond (AstVar var) mem = case readMemory mem var of
    Just (AstBool _) -> Right (AstVar var, mem)
    _ -> Left ("Unknown variable \"" ++ var ++ "\", no bool found.")
evalLoopCond (AstFunc func) mem = case readMemory mem (fName func) of
    Just (AstDefineFunc f) ->
        if fType f == Bool
            then Right (AstFunc func, mem)
            else Left ("Function \"" ++ fName f ++ "\" is not a function of type bool.")
    _ -> Left ("Unknown call to function \"" ++ fName func ++ "\".")
evalLoopCond (AstBool True) mem = Right (AstBool True, mem)
evalLoopCond (AstBool False) _ = Left "While conditions with a 'false' boolean aren't valid."
evalLoopCond (AstBinaryFunc op left right) mem =
    either
        (\err -> Left ("Expecting condition in while: " ++ err))
        ( \() -> Right (AstBinaryFunc op left right, mem)
        )
        (evalBinaryRet op Bool mem)
evalLoopCond _ _ = Left "Condition is not of bool type."

{- | Evaluate loop instances, creating a scope to handle features like:
>>> break
>>> continue
-}
evalLoops :: Ast -> Memory -> Either String (Ast, Memory)
evalLoops (AstLoop (Just loopName) cond block) mem =
    evalLoopCond cond mem >>= \(_, mem') ->
        case evalLoopBlock loopName (extractBlock block) mem' of
            Right (newBlock, _) ->
                Right (AstLoop (Just loopName) cond (AstBlock newBlock), mem')
            Left err -> Left err
evalLoops _ _ = Left "Invalid loop call."

----- Declarations (Defined Variables/ Defined Structures)

-- | Evaluate definition of variables by justifying type and value.
evalDefinition :: Ast -> MarylType -> Memory -> Either String Ast
evalDefinition AstVoid (Struct expectedType) mem =
    case readMemory mem expectedType of
        Just (AstDefineStruct struct) -> normalizeStruct (AstDefineStruct struct) AstVoid mem
        _ -> Left ("Struct of type " ++ expectedType ++ " can't be found.")
evalDefinition AstVoid _ _ = Right AstVoid
evalDefinition (AstArg ast _) expectedType mem = evalDefinition ast expectedType mem
evalDefinition (AstVar str) expectedType mem = case readMemory mem str of
    Just value -> evalDefinition value expectedType mem
    Nothing -> Left ("Variable " ++ str ++ " out of scope.")
evalDefinition (AstListElem listVar idx) expectedType mem = evalListElemDef listVar idx expectedType mem
evalDefinition (AstList eles) (List typeVar) mem
    | checkListType eles typeVar mem = Right (AstList eles)
    | otherwise = Left ("Element in list isn't of proper type, expected list full of " ++ show typeVar ++ ".")
evalDefinition (AstDefineVar origVar@(Variable varName varType _)) expectedType _
    | varType == expectedType = Right (AstDefineVar origVar)
    | otherwise = Left (varName ++ " isn't of proper type, expected " ++ show varType ++ ".")
evalDefinition ast (Struct structType) mem =
    case readMemory mem structType of
        Just (AstDefineStruct struct) -> normalizeStruct (AstDefineStruct struct) ast mem
        _ -> Left ("Struct of type " ++ structType ++ " can't be found.")
evalDefinition (AstBinaryFunc "." left right) expectedType mem =
    either Left Right (evalCallStructEle left right expectedType mem)
evalDefinition (AstBinaryFunc op left right) expectedType mem =
    either Left (\() -> Right (AstBinaryFunc op left right)) (evalBinaryRet op expectedType mem)
evalDefinition (AstFunc funcName) _ _ =
    trace "to do: evaluate functions " Right (AstFunc funcName)
evalDefinition (AstTernary cond doState elseState) expect mem =
    evalNode mem (AstTernary cond doState elseState) >>= \(_, mem') ->
        case evalDefinition doState expect mem' of
            Right _ -> evalDefinition elseState expect mem'
            _ -> Left (show doState ++ " in ternary isn't typed correctly, expected " ++ show expect ++ ".")
evalDefinition ast expectedType _
    | isValidType ast expectedType = Right ast
    | otherwise =
        Left (show ast ++ " isn't typed correctly, expected " ++ show expectedType ++ ".")

-- | Evaluate the definition of a struct type.
evalStructDecla :: [Ast] -> Memory -> Either String ()
evalStructDecla ((AstDefineVar (Variable _ varType varValue)) : xs) mem =
    either Left (const (evalStructDecla xs mem)) (evalDefinition varValue varType mem)
evalStructDecla [] _ = Right ()
evalStructDecla _ _ = Left "Invalid definition of structure."

evalCallStructEle :: Ast -> Ast -> MarylType -> Memory -> Either String Ast
evalCallStructEle left right expectedType mem = -- !! TO DO
    Left "Evaluator doesn't handle \".\" operator (call to structure element) at the moment."

----- Memory-based definitions

-- | Define a function into environment
addDefineFunc :: Memory -> Function -> Either String (Ast, Memory)
addDefineFunc mem func@(Function funcName args body _) =
    either
        (\err -> Left ("Failed to define function (" ++ err ++ ")."))
        ( \newMem ->
            evalArgs args (freeMemory newMem) >>= \editedMem ->
                evalASTBlock editedMem body >>= \(evaluatedBody, mem') ->
                    let func' = func {fBody = evaluatedBody}
                     in Right (AstVoid, updateMemory mem' funcName (AstDefineFunc func'))
        )
        (addMemory mem funcName (AstDefineFunc func))

addLoopLabel :: String -> Ast -> Ast -> Memory -> Memory
addLoopLabel loopName cond block mem =
    fromRight mem (addMemory mem loopName (AstGlobal (AstLoop (Just loopName) cond block)))

-- | Define a variable in scope
addDefineVar :: Memory -> Variable -> Either String (Ast, Memory)
addDefineVar mem var@(Variable varName _ vVal) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName evaluatedExpr of
            Right finalMem -> Right (AstDefineVar var, finalMem)
            Left err -> Left ("Failed to define variable (" ++ err ++ ").")

-- | Define a const global value
defineConstGlobal :: Memory -> Variable -> Either String (Ast, Memory)
defineConstGlobal mem var@(Variable varName _ vVal) =
    evalNode mem vVal >>= \(evaluatedExpr, updatedMem) ->
        case addMemory updatedMem varName (AstGlobal evaluatedExpr) of
            Right finalMem -> Right (AstGlobal (AstDefineVar var), finalMem)
            Left err -> Left ("Failed to define variable (" ++ err ++ ").")

-- | Add a global variable to memory, enforcing it to be typed as const.
addGlobalVar :: Memory -> Variable -> Either String (Ast, Memory)
addGlobalVar _ (Variable _ (Const _) AstVoid) = Left "Uninitialised const value."
addGlobalVar mem var@(Variable varName (Const varType) varValue)
    | isValidType varValue varType = defineConstGlobal mem var
    | otherwise =
        case evalDefinition varValue varType mem of
            Right (AstStruct eles) -> defineConstGlobal mem (Variable varName (Const varType) (AstStruct eles))
            Right _ -> defineConstGlobal mem var
            Left err -> Left (varName ++ " can't be defined: " ++ err)
addGlobalVar _ ast = Left (show ast ++ " can't be defined as global.<")

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
evalNode mem (AstConst (AstDefineVar var)) =
    evalNode mem (AstDefineVar var) >>= \(_, _) ->
        Right (AstConst (AstDefineVar var), updateMemory mem (vName var) (AstConst (AstDefineVar var)))
evalNode mem (AstVar name) =
    maybe
        (Left (name ++ " is an undefined variable"))
        (\value -> Right (value, mem))
        (readMemory mem name)
evalNode mem (AstDefineVar (Variable varName (Const varType) varValue)) =
    evalNode mem (AstConst (AstDefineVar (Variable varName varType varValue)))
evalNode mem (AstDefineVar var@(Variable varName varType varValue))
    | isValidType varValue varType = addDefineVar mem var
    | otherwise = case evalDefinition varValue varType mem of
            Right (AstStruct eles) -> addDefineVar mem (Variable varName varType (AstStruct eles))
            Right _ -> addDefineVar mem var
            Left err -> Left (varName ++ " can't be defined: " ++ err)
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
    evalNode mem cond
        >>= uncurry
            (evalIfConditions (AstIf cond trueBranch elseIfBranches elseBranch))
evalNode mem (AstTernary cond left right) =
    evalNode mem (AstIf cond left [] (Just right)) >> Right (AstTernary cond left right, mem)
evalNode mem (AstListElem var idxs) = evalList var idxs mem
evalNode mem (AstDefineStruct struct@(Structure name properties)) =
    either
        (\err -> Left ("Failed to define structure (" ++ err ++ ")."))
        ( \newMem ->
            evalStructDecla properties newMem >>= \() ->
                Right (AstDefineStruct struct, newMem)
        )
        (addMemory mem name (AstDefineStruct struct))
evalNode mem (AstReturn expr) = evalNode mem expr >>= \(_, mem') -> Right (AstReturn expr, mem')
evalNode mem node = Right (node, mem)

-- | Evaluate a list of AST nodes.
evalASTBlock :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalASTBlock mem (ast : asts) =
    evalNode mem ast >>= \(transformedAst, updatedMem) ->
        evalASTBlock updatedMem asts >>= \(rest, finalMem) ->
            Right (transformedAst : rest, finalMem)
evalASTBlock mem [] = Right ([], mem)

-- | Evaluate the initial parsed list of AST nodes.
evalAST :: Memory -> [Ast] -> Either String ([Ast], Memory)
evalAST mem asts = evalAST' mem asts D.empty

evalAST' :: Memory -> [Ast] -> D.DList Ast -> Either String ([Ast], Memory)
evalAST' mem [] acc = Right (D.toList acc, mem)
evalAST' mem (AstDefineFunc func : asts) acc =
    either
        (\err -> Left (show (AstDefineFunc func) ++ ":\n\t |- " ++ err))
        ( \(transformedAst, updatedMem) ->
            evalAST' updatedMem asts (acc `D.snoc` transformedAst)
        )
        (evalNode mem (AstDefineFunc func))
evalAST' mem (AstDefineStruct struct@(Structure name properties) : asts) acc =
    either
        (\err -> Left (show (AstDefineStruct struct) ++ ":\n\t |- Failed to define structure (" ++ err ++ ")."))
        ( \newMem ->
            evalStructDecla properties newMem >>= \() ->
                evalAST' newMem asts (acc `D.snoc` AstDefineStruct struct)
        )
        (addMemory mem name (AstGlobal (AstDefineStruct struct)))
evalAST' mem (AstDefineVar var@(Variable _ (Const _) _) : asts) acc =
    either
        (\err -> Left (show (AstDefineVar var) ++ ":\n\t |- " ++ err))
        ( \(transformedAst, updatedMem) ->
            evalAST' updatedMem asts (acc `D.snoc` transformedAst)
        )
        (addGlobalVar mem var)
evalAST' _ (ast : _) _ = Left (show ast ++ ":\n\t |- Can't define a global value that isn't const.")

-- | Helper to extract block contents.
extractBlock :: Ast -> [Ast]
extractBlock (AstBlock body) = body
extractBlock _ = []
