{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.Translation.ASTtoASM (translateToASM, translateAST) where

import Compiler.Translation.Functions (isBuiltin, isSingleOp, translateOpInst)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Eval.Assignment (normalizeStruct)
import qualified Data.DList as D
import Memory (Memory, addMemory, freeMemory, generateUniqueElseName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Structure (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, get, jump, jumpf, load, noop, push, pushArg, ret)

------- Operators

-- (= assignment operator)
handleAssignment :: Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
handleAssignment (AstVar var) right mem =
    (fst (translateAST right mem) `D.append` D.singleton (load Nothing var), mem)
handleAssignment (AstListElem var [x]) right mem =
    let prefixInstrs = D.fromList [get Nothing var, push Nothing (N (fromIntegral x))]
        (rightInstrs, updatedMem) = translateAST right mem
        postfixInstrs = D.fromList [call Nothing "set", load Nothing var]
     in (prefixInstrs `D.append` rightInstrs `D.append` postfixInstrs, updatedMem)
-- handleAssignment (AstListElem var (x : xs)) right mem =
-- handleAssignment (AstArg arg (Just idx)) right mem =
--     (fst (translateAST right mem))
-- struct
handleAssignment _ _ mem = (D.empty, mem)

updateAssignment :: String -> Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
updateAssignment ">>=" left right mem = handleAssignment left (AstBinaryFunc ">>" left right) mem
updateAssignment "<<=" left right mem = handleAssignment left (AstBinaryFunc "<<" left right) mem
updateAssignment (op : _) left right mem = handleAssignment left (AstBinaryFunc [op] left right) mem
updateAssignment _ _ _ mem = (D.empty, mem)

handlePriority :: Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
handlePriority left (AstFunc func) mem = (fst (translateAST (AstFunc func) mem) `D.append` fst (translateAST left mem), mem)
handlePriority left (AstArg arg idx) mem = (fst (translateAST (AstArg arg idx) mem) `D.append` fst (translateAST left mem), mem)
handlePriority left right mem =
    (fst (translateAST left mem) `D.append` fst (translateAST right mem), mem)

translateBinaryFunc :: String -> Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
translateBinaryFunc op left right mem
    | isSingleOp op = (fst (handlePriority left right mem) `D.append` D.singleton (translateOpInst op), mem)
    | otherwise = updateAssignment op left right mem

------- Structures

translateStruct :: Ast -> Ast -> String -> Memory -> (D.DList Instruction, Memory)
translateStruct (AstDefineStruct struct@(Structure structName props)) AstVoid varName mem =
    case mapM toStructField props of
        Just fields ->
            (D.singleton (push Nothing (St $ Map.fromList fields)), updateMemory mem varName (AstStruct props))
        Nothing -> (D.empty, mem)
    where
      toStructField (AstDefineVar (Variable n _ v)) =
              fmap ((,) n) (associateTypes v mem)
      toStructField _ = Nothing
translateStruct (AstDefineStruct struct) structValues nameStruct mem =
    case normalizeStruct (AstDefineStruct struct) structValues of
        Right (AstStruct eles) -> trace (">>> " ++ nameStruct ++ " " ++ show eles) $
             case mapM toStructField eles of
                Just fields ->
                    (D.fromList [push Nothing (St $ Map.fromList fields), load Nothing nameStruct],
                        updateMemory mem nameStruct (AstStruct eles))
                Nothing -> (D.empty, mem)
            where
                toStructField (AstLabel fieldName fieldValue) =
                    fmap (\val -> (fieldName, val)) (associateTypes fieldValue mem)
                toStructField _ = Nothing
        _ -> (D.empty, mem)
translateStruct  _ _ _ mem = (D.empty, mem)

------- Lists

-- add structures
associateTypes :: Ast -> Memory -> Maybe Value
associateTypes (AstInt n) _ = Just (N (fromIntegral n))
associateTypes (AstBool b) _ = Just (B b)
associateTypes (AstString s) _ = Just (S s)
associateTypes (AstDouble d) _ = Just (D d)
associateTypes (AstChar c) _ = Just (C c)
associateTypes (AstList list) mem = Just (L (translateList list mem))
associateTypes (AstListElem var _) mem = case readMemory mem var of -- check this
    Just (AstList (x : _)) -> associateTypes x mem
    _ -> Nothing
associateTypes (AstArg ast _) mem = associateTypes ast mem
associateTypes (AstVar var) mem = case readMemory mem var of
    Just val -> associateTypes val mem
    _ -> Nothing
associateTypes _ _ = Nothing

translateList :: [Ast] -> Memory -> [Value]
translateList [] _ = []
translateList (x : xs) mem = case associateTypes x mem of
    Just val -> val : translateList xs mem
    _ -> []

translateMultIndexes :: [Int] -> Memory -> D.DList Instruction
translateMultIndexes (x : xs) mem =
    fst (translateAST (AstInt x) mem) `D.append`
    D.singleton (call Nothing "get") `D.append` translateMultIndexes xs mem
translateMultIndexes [] _ = D.empty

------- AstIf / Ternary

-- (blocks in scope, length for jump/jumpf)
translateBlock' :: Ast -> Memory -> (D.DList Instruction, Int)
translateBlock' (AstBlock block) mem =
    let (instructions, _) = translateToASM block mem
     in (fst (translateToASM block mem), length instructions)
translateBlock' ast mem =
    let (instructions, _) = translateAST ast mem
     in (instructions, length instructions)

-- (if condition block branches)
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

-- (else if branches)
translateAllConditions :: [Ast] -> Memory -> String -> (D.DList Instruction, Memory)
translateAllConditions [] mem _ = (D.empty, mem)
translateAllConditions (AstIf cond block _ _ : rest) mem elseLabel =
    let (condInstructions, memAfterCond) = translateConditionBlock cond block mem elseLabel
        (restInstructions, finalMem) = translateAllConditions rest memAfterCond elseLabel
     in (condInstructions `D.append` restInstructions, finalMem)
translateAllConditions _ mem _ = (D.empty, mem)

translateIf :: Ast -> Memory -> (D.DList Instruction, Memory)
translateIf (AstIf cond ifBlock elseifEles elseEle) mem =
    let elseName = generateUniqueElseName mem
        newMemResult = addMemory mem elseName (AstIf cond ifBlock elseifEles elseEle)
     in case newMemResult of
            Right newMem ->
                let (condInstructions, memAfterCond) = translateConditionBlock cond ifBlock newMem elseName
                    (elseifInstructions, memAfterIfElse) = translateAllConditions elseifEles memAfterCond elseName
                    (elseInstructions, memAfterElse) = maybe (D.empty, mem) (`translateAST` memAfterIfElse) elseEle
                 in (condInstructions `D.append` elseifInstructions `D.append` elseInstructions `D.append`
                    D.singleton (noop (Just $ "." ++ elseName)), memAfterElse)
            _ -> (D.empty, mem)
translateIf _ mem = (D.empty, mem)

------- Loops

translateLoop :: String -> Ast -> Ast -> Memory -> (D.DList Instruction, Memory)
translateLoop loopName cond block mem =
    let (condInstructions, memAfterCond) = translateAST cond mem
        (blockInstructions, _) = translateAST block memAfterCond
     in (D.singleton (noop (Just $ "." ++ loopName)) `D.append` condInstructions `D.append` D.singleton (jumpf Nothing (Right ("end" ++ loopName)))
        `D.append` blockInstructions `D.append` D.fromList [jump Nothing (Right loopName), noop (Just $ ".end" ++ loopName)], memAfterCond)
 
------- Functions

translateBuiltin :: String -> [Ast] -> Memory -> (D.DList Instruction, Memory)
translateBuiltin n' args mem =
    ( fst (translateArgs args mem) `D.append` D.singleton (call Nothing n'),
      mem
    )

-- (call defined Vars)
callArgs :: Ast -> Memory -> D.DList Instruction
callArgs (AstVar varName) mem =
    case readMemory mem varName of
        Just (AstArg ast n) -> fst $ translateAST (AstArg ast n) mem
        Just _ -> D.singleton (get Nothing varName)
        Nothing -> D.empty
callArgs ast mem = fst $ translateAST ast mem

-- (parsing AstVar)
translateArgs :: [Ast] -> Memory -> (D.DList Instruction, Memory)
translateArgs [] mem = (D.empty, mem)
translateArgs (x : xs) mem =
    (callArgs x mem `D.append` fst (translateArgs xs mem), mem) -- called args

pushArgs :: [Ast] -> Memory -> Int -> Memory
pushArgs [] mem _ = mem
pushArgs (AstDefineVar (Variable varName varType varValue) : xs) mem idx =
    let updatedMem = updateMemory mem varName (AstArg (AstDefineVar (Variable varName varType varValue)) (Just idx))
     in pushArgs xs updatedMem (idx + 1)
pushArgs _ mem _ = mem

-------

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

translateToASM :: [Ast] -> Memory -> (D.DList Instruction, Memory)
translateToASM asts mem = (instructions, finalMem)
  where
    (instructions, finalMem) = foldl processAST (D.empty, mem) asts

    processAST :: (D.DList Instruction, Memory) -> Ast -> (D.DList Instruction, Memory)
    processAST (instrs, currentMem) ast =
        let (newInstrs, updatedMem) = translateAST ast currentMem
         in (instrs `D.append` newInstrs, updatedMem)
