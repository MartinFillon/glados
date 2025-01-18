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
import Memory (Memory, addMemory, freeMemory, generateUniqueElseName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Structure (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, get, jump, jumpf, load, noop, push, pushArg, ret)
import Eval.Assignment (getIndexes)
import Data.Either (fromRight)
import Data.Maybe (fromJust)

------- Operators

-- (= assignment operator)
handleAssignment :: Ast -> Ast -> Memory -> ([Instruction], Memory)
handleAssignment (AstVar var) right mem =
    (fst (translateAST right mem) ++ [load Nothing var], mem)
handleAssignment (AstListElem var [x]) right mem =
    ([get Nothing var, push Nothing (N (fromIntegral (head $ fromRight [0] (getIndexes mem [x]))))] ++ fst (translateAST right mem) ++
        [call Nothing "set", load Nothing var], mem)
-- handleAssignment (AstListElem var (x : xs)) right mem =
-- handleAssignment (AstArg arg (Just idx)) right mem =
--     (fst (translateAST right mem))
-- struct
handleAssignment _ _ mem = ([], mem)

updateAssignment :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
updateAssignment ">>=" left right mem = handleAssignment left (AstBinaryFunc ">>" left right) mem
updateAssignment "<<=" left right mem = handleAssignment left (AstBinaryFunc "<<" left right) mem
updateAssignment (op : _) left right mem = handleAssignment left (AstBinaryFunc [op] left right) mem
updateAssignment _ _ _ mem = ([], mem)

handlePriority :: Ast -> Ast -> Memory -> ([Instruction], Memory)
handlePriority left (AstFunc func) mem = (fst (translateAST (AstFunc func) mem) ++ fst (translateAST left mem), mem)
handlePriority left (AstArg arg idx) mem = (fst (translateAST (AstArg arg idx) mem) ++ fst (translateAST left mem), mem)
handlePriority left right mem =
    (fst (translateAST left mem) ++ fst (translateAST right mem), mem)

translateBinaryFunc :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
translateBinaryFunc op left right mem
    | isSingleOp op = (fst (handlePriority left right mem) ++ [translateOpInst op], mem)
    | otherwise = updateAssignment op left right mem

------- Structures

-- translateStructLabels :: [Ast] -> Memory -> ([Instruction], Memory)
-- translateStructLabels [] mem = ([], mem)
-- translateStructLabels (AstLabel fieldName fieldValue: xs) mem =

-- translateStruct :: [Ast] -> Ast -> Memory -> ([Instruction], Memory)
-- translateStruct [] (AstDefineStruct struct) mem = ([], mem)
-- translateStruct

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

translateMultIndexes :: [Ast] -> Memory -> [Instruction]
translateMultIndexes (x : xs) mem =
    fst (translateAST x mem) ++ [call Nothing "get"] ++ translateMultIndexes xs mem
translateMultIndexes [] _ = []

------- AstIf / Ternary

-- (blocks in scope, length for jump/jumpf)
translateBlock' :: Ast -> Memory -> ([Instruction], Int)
translateBlock' (AstBlock block) mem =
    let (instructions, _) = translateToASM block mem
     in (fst (translateToASM block mem), length instructions)
translateBlock' ast mem =
    let (instructions, _) = translateAST ast mem
     in (instructions, length instructions)

-- (if condition block branches)
translateConditionBlock :: Ast -> Ast -> Memory -> String -> ([Instruction], Memory)
translateConditionBlock cond block mem elseLabel =
    let (condInstructions, memAfterCond) = translateAST cond mem
        (blockInstructions, blockLength) = translateBlock' block memAfterCond
        allInstructions =
            condInstructions
                ++ [jumpf Nothing (Left (blockLength + 1))]
                ++ blockInstructions
                ++ [jump Nothing (Right elseLabel)]
     in (allInstructions, memAfterCond)

-- (else if branches)
translateAllConditions :: [Ast] -> Memory -> String -> ([Instruction], Memory)
translateAllConditions [] mem _ = ([], mem)
translateAllConditions (AstIf cond block _ _ : rest) mem elseLabel =
    let (condInstructions, memAfterCond) = translateConditionBlock cond block mem elseLabel
        (restInstructions, finalMem) = translateAllConditions rest memAfterCond elseLabel
     in (condInstructions ++ restInstructions, finalMem)
translateAllConditions _ mem _ = ([], mem)

translateIf :: Ast -> Memory -> ([Instruction], Memory)
translateIf (AstIf cond ifBlock elseifEles elseEle) mem =
    let elseName = generateUniqueElseName mem
        newMemResult = addMemory mem elseName (AstIf cond ifBlock elseifEles elseEle)
     in case newMemResult of
            Right newMem ->
                let (condInstructions, memAfterCond) = translateConditionBlock cond ifBlock newMem elseName
                    (elseifInstructions, memAfterIfElse) = translateAllConditions elseifEles memAfterCond elseName
                    (elseInstructions, memAfterElse) = maybe ([], mem) (`translateAST` memAfterIfElse) elseEle
                 in (condInstructions ++ elseifInstructions ++ elseInstructions ++ [noop (Just $ "." ++ elseName)], memAfterElse)
            _ -> ([], mem)
translateIf _ mem = ([], mem)

------- Loops

translateLoop :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
translateLoop loopName cond block mem =
    let (condInstructions, memAfterCond) = translateAST cond mem
        (blockInstructions, _) = translateAST block memAfterCond
     in ([noop (Just $ "." ++ loopName)] ++ condInstructions ++ [jumpf Nothing (Right ("end" ++ loopName))] ++
        blockInstructions ++ [jump Nothing (Right loopName), noop (Just $ ".end" ++ loopName)], memAfterCond)

------- Functions

translateBuiltin :: String -> [Ast] -> Memory -> ([Instruction], Memory)
translateBuiltin n' args mem =
    ( fst (translateArgs args mem) ++ [call Nothing n'],
      mem
    )

-- (call defined Vars)
callArgs :: Ast -> Memory -> [Instruction]
callArgs (AstVar varName) mem =
    case readMemory mem varName of
        Just (AstArg ast n) -> fst $ translateAST (AstArg ast n) mem
        Just _ -> [get Nothing varName]
        Nothing -> []
callArgs ast mem = fst $ translateAST ast mem

-- (parsing AstVar)
translateArgs :: [Ast] -> Memory -> ([Instruction], Memory)
translateArgs [] mem = ([], mem)
translateArgs (x : xs) mem =
    (callArgs x mem ++ fst (translateArgs xs mem), mem) -- called args

pushArgs :: [Ast] -> Memory -> Int -> Memory
pushArgs [] mem _ = mem
pushArgs (AstDefineVar (Variable varName varType varValue) : xs) mem idx =
    let updatedMem = updateMemory mem varName (AstArg (AstDefineVar (Variable varName varType varValue)) (Just idx))
     in pushArgs xs updatedMem (idx + 1)
pushArgs _ mem _ = mem

-------

translateAST :: Ast -> Memory -> ([Instruction], Memory)
translateAST (AstArg _ (Just n)) mem = ([pushArg Nothing n], mem)
translateAST (AstArg (AstDefineVar (Variable varName _ _)) Nothing) mem =
    case readMemory mem varName of
        Just val -> translateAST val mem
        Nothing -> ([], mem)
-- translateAST (AstDefineVar (Variable varName (Struct structName) (AstStruct eles))) mem =
--     case readMemory mem structName of
--         Just (AstDefineStruct struct) -> translateStruct eles (AstDefineStruct struct) mem
--         _ -> ([], mem)
translateAST (AstDefineVar (Variable varName _ varValue)) mem =
    (fst (translateAST varValue mem) ++ [load Nothing varName], updateMemory mem varName varValue)
translateAST (AstVar varName) mem = (callArgs (AstVar varName) mem, mem)
translateAST (AstDefineFunc (Function _ funcArgs funcBody _)) mem =
    let newMem = pushArgs funcArgs (freeMemory mem) 0
     in (fst (translateToASM funcBody newMem), newMem)
translateAST (AstFunc (Function funcName funcArgs _ _)) mem
    | isBuiltin funcName = translateBuiltin funcName funcArgs mem
    | otherwise =
        ( fst (translateArgs funcArgs mem) ++ [call Nothing ('.' : funcName)],
          mem
        )
translateAST (AstReturn ast) mem = (fst (translateAST ast mem) ++ [ret Nothing], mem)
translateAST (AstPrefixFunc (op : _) ast) mem = handleAssignment ast (AstBinaryFunc [op] ast (AstInt 1)) mem -- check differences
translateAST (AstPostfixFunc (op : _) ast) mem = handleAssignment ast (AstBinaryFunc [op] ast (AstInt 1)) mem
translateAST (AstBinaryFunc "=" left right) mem = handleAssignment left right mem
translateAST (AstBinaryFunc op left right) mem = translateBinaryFunc op left right mem
translateAST (AstTernary cond doBlock elseBlock) mem = translateAST (AstIf cond doBlock [] (Just elseBlock)) mem
translateAST (AstIf cond ifBlock elseifEles elseEle) mem = translateIf (AstIf cond ifBlock elseifEles elseEle) mem
translateAST (AstLoop (Just loopName) cond block) mem = translateLoop loopName cond block mem
translateAST (AstBlock block) mem = translateToASM block mem
translateAST (AstBreak (Just loopName)) mem = ([jump Nothing (Right ("end" ++ loopName))], mem)
translateAST (AstContinue (Just loopName)) mem = ([jump Nothing (Right loopName)], mem)
translateAST (AstInt n) mem = ([push Nothing (N (fromIntegral n))], mem)
translateAST (AstBool b) mem = ([push Nothing (B b)], mem)
translateAST (AstString s) mem = ([push Nothing (S s)], mem)
translateAST (AstDouble d) mem = ([push Nothing (D d)], mem)
translateAST (AstChar c) mem = ([push Nothing (C c)], mem)
translateAST (AstList list) mem = ([push Nothing (L (translateList list mem))], mem)
translateAST (AstListElem var idxs) mem =
    (fst (translateAST (AstVar var) mem) ++ translateMultIndexes idxs mem, mem)
translateAST (AstDefineStruct struct@(Structure structName props)) mem =
    case mapM toStructField props of
        Just fields ->
            ([push Nothing (St $ Map.fromList fields)], updateMemory mem structName (AstDefineStruct struct))
        Nothing -> ([], mem)
    where
      toStructField (AstDefineVar (Variable n _ v)) =
          trace ("Processing field: " ++ n) $
              fmap ((,) n) (associateTypes v mem)
      toStructField _ = Nothing
-- translateAST (AstStruct eles) mem =
translateAST _ mem = ([], mem)

translateToASM :: [Ast] -> Memory -> ([Instruction], Memory)
translateToASM asts mem = foldl processAST ([], mem) asts
  where
    processAST (instructions, currentMem) ast =
        let (newInstructions, updatedMem) = translateAST ast currentMem
         in (instructions ++ newInstructions, updatedMem)

-- translateToASM :: [Ast] -> Memory -> ([Instruction], Memory)
-- translateToASM asts mem = (toList instructions, finalMem)
--   where
--     (instructions, finalMem) = foldl processAST (empty, mem) asts
--     processAST (instrs, currentMem) ast =
--         let (newInstrs, updatedMem) = translateAST ast currentMem
--          in (instrs `append` singleton newInstrs, updatedMem)
