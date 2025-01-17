{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}
{-# LANGUAGE TupleSections #-}

module Compiler.Translation.ASTtoASM (translateToASM, translateAST) where

import Compiler.Translation.Functions (isBuiltin, isSingleOp, pushArgs, translateOpInst)
import Compiler.Translation.ListsStructures (associateTypes, translateList, translateMultIndexes)
import qualified Data.DList as D
import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Structures (normalizeStruct)
import Memory (Memory, addMemory, freeMemory, generateUniqueElseName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Structure (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, get, jump, jumpf, load, noop, push, pushArg, ret)

------- Operators

{- | Translates a 'set' operation on a list element for nested or multi-dimensional lists.
 Handles variable assignment when the list contains more than one dimension.
-}
setMultipleIndexes :: [Int] -> Int -> String -> Ast -> Memory -> (D.DList Instruction, String)
setMultipleIndexes [x] n baseVarName ast mem =
    ( D.singleton (push Nothing (N (fromIntegral x)))
        `D.append` fst (translateAST ast mem)
        `D.append` D.fromList
            [call Nothing "set", load Nothing (baseVarName ++ show n)],
      baseVarName ++ show n
    )
setMultipleIndexes (x : xs) 0 baseVarName ast mem =
    let (nestedInstructions, lastVarName) = setMultipleIndexes xs 1 baseVarName ast mem
    in ( D.fromList [push Nothing (N (fromIntegral x)), call Nothing "get"]
            `D.append` nestedInstructions,
          lastVarName
        )
setMultipleIndexes (x : xs) n baseVarName ast mem =
    let (nestedInstructions, lastVarName) = setMultipleIndexes xs (n + 1) baseVarName ast mem
     in ( D.fromList [push Nothing (N (fromIntegral x)), call Nothing "get", load Nothing (baseVarName ++ show n)]
            `D.append` nestedInstructions,
          lastVarName
        )
setMultipleIndexes [] _ _ _ _ = (D.empty, "")

{- Assignment operator '='
 Supports regular variables, normal list element call and nested list element call.
 Uses 'load' instruction on instance of redefinition of variable.
-}
handleAssignment :: Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
handleAssignment (AstVar var) right mem =
    (fst (translateAST right mem) `D.append` D.singleton (load Nothing var), mem)
handleAssignment (AstListElem var [x]) right mem =
    let prefixInstrs = D.fromList [get Nothing var, push Nothing (N (fromIntegral x))]
        (rightInstrs, updatedMem) = translateAST right mem
        postfixInstrs = D.fromList [call Nothing "set", load Nothing var]
     in (prefixInstrs `D.append` rightInstrs `D.append` postfixInstrs, updatedMem)
handleAssignment (AstListElem var list) right mem =
    let prefixInstrs = D.singleton (get Nothing var)
        (indexInstrs, lastVarName) = setMultipleIndexes list 0 var right mem
        postfixInstrs = D.fromList [get Nothing var, push Nothing (N 0), get Nothing lastVarName, call Nothing "set", load Nothing var]
     in (prefixInstrs `D.append` indexInstrs `D.append` postfixInstrs, mem)
-- handleAssignment (AstArg arg (Just idx)) right mem =
--     (fst (translateAST right mem))
-- struct
handleAssignment _ _ mem = (D.empty, mem)

{- | Handles compound assignment operators such as '+=', '-=', etc.,
 by converting them to their equivalent binary operation and delegating to handleAssignment.
-}
updateAssignment :: String -> Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
updateAssignment ">>=" left right mem = handleAssignment left (AstBinaryFunc ">>" left right) mem
updateAssignment "<<=" left right mem = handleAssignment left (AstBinaryFunc "<<" left right) mem
updateAssignment (op : _) left right mem = handleAssignment left (AstBinaryFunc [op] left right) mem
updateAssignment _ _ _ mem = (D.empty, mem)

-- | Priority is considered on calls to functions/ function argument to compute before call to operation
handlePriority :: Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
handlePriority left (AstFunc func) mem = (fst (translateAST (AstFunc func) mem) `D.append` fst (translateAST left mem), mem)
handlePriority left (AstArg arg idx) mem = (fst (translateAST (AstArg arg idx) mem) `D.append` fst (translateAST left mem), mem)
handlePriority left right mem =
    (fst (translateAST left mem) `D.append` fst (translateAST right mem), mem)

{- | Handles the translation of an AST binary function:
 such as arithmetic or logical operations.
-}
translateBinaryFunc :: String -> Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
translateBinaryFunc op left right mem
    | isSingleOp op = (fst (handlePriority left right mem) `D.append` D.singleton (translateOpInst op), mem)
    | otherwise = updateAssignment op left right mem

------- Structures

{- | Translates evaluated structures to map of tuple of field name and its value:

>>> [("x", 4), ("y", 2), ("z", 4)]
-}
translateStruct :: Ast -> Ast -> String -> Memory -> (D.DList Instruction, Memory)
translateStruct (AstDefineStruct (Structure _ props)) AstVoid varName mem =
    case mapM toStructField props of
        Just fields ->
            (D.singleton (push Nothing (St $ Map.fromList fields)), updateMemory mem varName (AstStruct props))
        Nothing -> (D.empty, mem)
  where
    toStructField :: Ast -> Maybe (String, Value)
    toStructField (AstDefineVar (Variable n _ v)) =
        fmap (n,) (associateTypes v mem)
    toStructField _ = Nothing
translateStruct (AstDefineStruct struct) structValues nameStruct mem =
    case normalizeStruct (AstDefineStruct struct) structValues of
        Right (AstStruct eles) ->
            case mapM toStructField eles of
                Just fields ->
                    ( D.fromList [push Nothing (St $ Map.fromList fields), load Nothing nameStruct],
                      updateMemory mem nameStruct (AstStruct eles)
                    )
                Nothing -> (D.empty, mem)
          where
            toStructField :: Ast -> Maybe (String, Value)
            toStructField (AstLabel fieldName fieldValue) =
                fmap (fieldName,) (associateTypes fieldValue mem)
            toStructField _ = Nothing
        _ -> (D.empty, mem)
translateStruct _ _ _ mem = (D.empty, mem)

------- AstIf / Ternary

{- | Translate blocks of code (in scope) and returns translation with the number of lines.
 Allows the use of jump/ jumpf instructions, to allow jumping past block.
-}
translateBlock' :: Ast -> Memory -> (D.DList Instruction, Int)
translateBlock' (AstBlock block) mem =
    let (instructions, _) = translateToASM block mem
     in (fst (translateToASM block mem), length instructions)
translateBlock' ast mem =
    let (instructions, _) = translateAST ast mem
     in (instructions, length instructions)

{- Groups the translation of a condition, the block and necessary jumps.
 Translates a condition,
 adds a 'jumpIfFalse' instruction to the lengths of the block,
 adds the translation of block,
 and a 'jump' to label of 'elseName' generated.

>>> <cond>, jumpf (length <block>), <block>, jump (<.elseName>)
-}
translateConditionBlock :: Ast -> Ast -> Memory -> String -> (D.DList Instruction, Memory)
translateConditionBlock cond block mem elseLabel =
    let (condInstructions, memAfterCond) = translateAST cond mem
        (blockInstructions, blockLength) = translateBlock' block memAfterCond
        allInstructions =
            condInstructions
                `D.append` D.singleton (jumpf Nothing (Left (blockLength + 1)))
                `D.append` blockInstructions
                `D.append` D.singleton (jump Nothing (Right elseLabel))
     in (allInstructions, memAfterCond)

-- | Recursive function that takes a list of ElseIf conditions and translates it
translateAllConditions :: [Ast] -> Memory -> String -> (D.DList Instruction, Memory)
translateAllConditions [] mem _ = (D.empty, mem)
translateAllConditions (AstIf cond block _ _ : rest) mem elseLabel =
    let (condInstructions, memAfterCond) = translateConditionBlock cond block mem elseLabel
        (restInstructions, finalMem) = translateAllConditions rest memAfterCond elseLabel
     in (condInstructions `D.append` restInstructions, finalMem)
translateAllConditions _ mem _ = (D.empty, mem)

-- | Groups the translation of a full AstIf instance, with else if and else.
translateIf :: Ast -> Memory -> (D.DList Instruction, Memory)
translateIf (AstIf cond ifBlock elseifEles elseEle) mem =
    let elseName = generateUniqueElseName mem
        newMemResult = addMemory mem elseName (AstIf cond ifBlock elseifEles elseEle)
     in case newMemResult of
            Right newMem ->
                let (condInstructions, memAfterCond) = translateConditionBlock cond ifBlock newMem elseName
                    (elseifInstructions, memAfterIfElse) = translateAllConditions elseifEles memAfterCond elseName
                    (elseInstructions, memAfterElse) = maybe (D.empty, mem) (`translateAST` memAfterIfElse) elseEle
                 in ( condInstructions
                        `D.append` elseifInstructions
                        `D.append` elseInstructions
                        `D.append` D.singleton (noop (Just $ "." ++ elseName)),
                      memAfterElse
                    )
            _ -> (D.empty, mem)
translateIf _ mem = (D.empty, mem)

------- Loops

{-
-}
translateLoop :: String -> Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
translateLoop loopName cond block mem =
    let (condInstructions, memAfterCond) = translateAST cond mem
        (blockInstructions, _) = translateAST block memAfterCond
     in ( D.singleton (noop (Just $ "." ++ loopName))
            `D.append` condInstructions
            `D.append` D.singleton (jumpf Nothing (Right ("end" ++ loopName)))
            `D.append` blockInstructions
            `D.append` D.fromList [jump Nothing (Right loopName), noop (Just $ ".end" ++ loopName)],
          memAfterCond
        )
 
------- Functions

-- | Highlights call to builtin functions (no '.' at name of function)
translateBuiltin :: String -> [Ast] -> Memory -> (D.DList Instruction, Memory)
translateBuiltin n' args mem =
    ( fst (translateArgs args mem) `D.append` D.singleton (call Nothing n'),
      mem
    )

-- | Call defined variables using instruction 'get'
callArgs :: Ast -> Memory -> D.DList Instruction
callArgs (AstVar varName) mem =
    case readMemory mem varName of
        Just (AstArg ast n) -> fst $ translateAST (AstArg ast n) mem
        Just _ -> D.singleton (get Nothing varName)
        Nothing -> D.empty
callArgs ast mem = fst $ translateAST ast mem

-- | Translation of args passed into a function call to push before instruction 'call'
translateArgs :: [Ast] -> Memory -> (D.DList Instruction, Memory)
translateArgs [] mem = (D.empty, mem)
translateArgs (x : xs) mem =
    (callArgs x mem `D.append` fst (translateArgs xs mem), mem) -- called args

-------

-- | Translates an AST node to its corresponding assembly instructions.
translateAST :: Ast -> Memory -> (D.DList Instruction, Memory)
translateAST (AstArg _ (Just n)) mem = (D.singleton (pushArg Nothing n), mem)
translateAST (AstArg (AstDefineVar (Variable varName _ _)) Nothing) mem =
    case readMemory mem varName of
        Just val -> translateAST val mem
        Nothing -> (D.empty, mem)
translateAST (AstDefineVar (Variable varName (Struct structName) ast)) mem =
    case readMemory mem structName of
        Just (AstDefineStruct struct) ->
            translateStruct (AstDefineStruct struct) ast varName mem
        _ -> (D.empty, mem)
translateAST (AstDefineVar (Variable varName _ varValue)) mem =
    let (instrs, updatedMem) = translateAST varValue mem
     in (instrs `D.append` D.singleton (load Nothing varName), updateMemory updatedMem varName varValue)
translateAST (AstVar varName) mem = (callArgs (AstVar varName) mem, mem)
translateAST (AstDefineFunc (Function _ funcArgs funcBody _)) mem =
    let newMem = pushArgs funcArgs (freeMemory mem) 0
     in (fst (translateToASM funcBody newMem), newMem)
translateAST (AstFunc (Function funcName funcArgs _ _)) mem
    | isBuiltin funcName = translateBuiltin funcName funcArgs mem
    | otherwise =
        ( fst (translateArgs funcArgs mem) `D.append` D.singleton (call Nothing ('.' : funcName)),
          mem
        )
translateAST (AstReturn ast) mem = (fst (translateAST ast mem) `D.append` D.singleton (ret Nothing), mem)
translateAST (AstPrefixFunc (op : _) ast) mem = handleAssignment ast (AstBinaryFunc [op] ast (AstInt 1)) mem -- check differences
translateAST (AstPostfixFunc (op : _) ast) mem = handleAssignment ast (AstBinaryFunc [op] ast (AstInt 1)) mem
translateAST (AstBinaryFunc "=" left right) mem = handleAssignment left right mem
translateAST (AstBinaryFunc op left right) mem = translateBinaryFunc op left right mem
translateAST (AstTernary cond doBlock elseBlock) mem = translateAST (AstIf cond doBlock [] (Just elseBlock)) mem
translateAST (AstIf cond ifBlock elseifEles elseEle) mem = translateIf (AstIf cond ifBlock elseifEles elseEle) mem
translateAST (AstLoop (Just loopName) cond block) mem = translateLoop loopName cond block mem
translateAST (AstBlock block) mem = translateToASM block mem
translateAST (AstBreak (Just loopName)) mem = (D.singleton (jump Nothing (Right ("end" ++ loopName))), mem)
translateAST (AstContinue (Just loopName)) mem = (D.singleton (jump Nothing (Right loopName)), mem)
translateAST (AstInt n) mem = (D.singleton (push Nothing (N (fromIntegral n))), mem)
translateAST (AstBool b) mem = (D.singleton (push Nothing (B b)), mem)
translateAST (AstString s) mem = (D.singleton (push Nothing (S s)), mem)
translateAST (AstDouble d) mem = (D.singleton (push Nothing (D d)), mem)
translateAST (AstChar c) mem = (D.singleton (push Nothing (C c)), mem)
translateAST (AstList list) mem = (D.singleton (push Nothing (L (translateList list mem))), mem)
translateAST (AstListElem var idxs) mem =
    (fst (translateAST (AstVar var) mem) `D.append` translateMultIndexes idxs mem, mem)
translateAST (AstDefineStruct struct@(Structure structName _)) mem =
    (D.empty, updateMemory mem structName (AstDefineStruct struct))
translateAST _ mem = (D.empty, mem)

-- | Translates a list of AST nodes to assembly instructions.
translateToASM :: [Ast] -> Memory -> (D.DList Instruction, Memory)
translateToASM asts mem = (instructions, finalMem)
  where
    (instructions, finalMem) = foldl processAST (D.empty, mem) asts

    processAST :: (D.DList Instruction, Memory) -> Ast -> (D.DList Instruction, Memory)
    processAST (instrs, currentMem) ast =
        let (newInstrs, updatedMem) = translateAST ast currentMem
         in (instrs `D.append` newInstrs, updatedMem)
