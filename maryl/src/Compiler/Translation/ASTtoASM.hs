{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.Translation.ASTtoASM (translateToASM, translateAST) where

import Compiler.Streamline (clarifyAST)
import Compiler.Translation.Functions (isBuiltin, isSingleOp, translateOpInst)
import Debug.Trace (trace)
import Eval.Assignment (updateList)
import Memory (Memory, addMemory, freeMemory, generateUniqueElseName, generateUniqueLoopName, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, jump, jumpf, noop, push, pushArg, ret)

------- =

-- (= assignment operator)
handleAssignment :: Ast -> Ast -> Memory -> ([Instruction], Memory)
handleAssignment (AstVar var) right mem =
    let clarifiedRight = clarifyAST right mem
        newMem = updateMemory mem var clarifiedRight
     in translateAST (AstVar var) newMem
handleAssignment (AstListElem var (x : xs)) right mem =
    case updateList var (AstListElem var (x : xs)) mem (clarifyAST right mem) of
        Right (clarified, updatedMem) ->
            let newMem = updateMemory updatedMem var clarified
                instructions =
                    concatMap
                        fst
                        [ translateAST (AstVar var) mem,
                          translateAST (AstInt x) mem,
                          translateAST right mem
                        ]
                        ++ [call Nothing "set"]
             in (instructions, newMem)
        _ -> ([], mem)
handleAssignment _ _ mem = ([], mem)

updateAssignment :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
updateAssignment ">>=" left right mem = handleAssignment left (AstBinaryFunc ">>" (clarifyAST left mem) right) mem
updateAssignment "<<=" left right mem = handleAssignment left (AstBinaryFunc "<<" (clarifyAST left mem) right) mem
updateAssignment (op : _) left right mem = handleAssignment left (AstBinaryFunc [op] (clarifyAST left mem) right) mem
-- updateAssignment "|=" left right mem =
-- updateAssignment "&=" left right mem =
-- updateAssignment "^=" left right mem =
updateAssignment _ _ _ mem = ([], mem)

------- AstIf / Ternary

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
translateAllConditions (AstIf cond block elseIfs elseBlock : rest) mem elseLabel =
    let (condInstructions, memAfterCond) = translateConditionBlock cond block mem elseLabel
        (restInstructions, finalMem) = translateAllConditions rest memAfterCond elseLabel
     in (condInstructions ++ restInstructions, finalMem)
translateAllConditions _ mem _ = ([], mem)

------- Operators

translateBinaryFunc :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
translateBinaryFunc op left right mem
    | isSingleOp op = (fst (translateAST left mem) ++ fst (translateAST right mem) ++ [translateOpInst op], mem)
    | otherwise = updateAssignment op left right mem

------- AstVar (for arguments and defined variables)

-- (call defined Vars)
callArgs :: Ast -> Memory -> [Instruction]
callArgs (AstVar varName) mem =
    maybe [] (\ast -> fst $ translateAST ast mem) (readMemory mem varName)
callArgs ast mem = fst $ translateAST ast mem

-- (parsing AstVar)
translateArgs :: [Ast] -> Memory -> Int -> Bool -> ([Instruction], Memory)
translateArgs [] mem _ _ = ([], mem)
translateArgs (x : xs) mem n False =
    (callArgs x mem ++ fst (translateArgs xs mem n False), mem) -- called args
translateArgs (_ : xs) mem n True =
    (pushArg Nothing n : fst (translateArgs xs mem (n + 1) True), mem) -- defined args (pusharg)

------- AstBlock

-- (blocks in scope, length for jump/jumpf)
translateBlock' :: Ast -> Memory -> ([Instruction], Int)
translateBlock' (AstBlock block) mem =
    let (instructions, _) = translateToASM block mem
     in (fst (translateToASM block mem), length instructions)
translateBlock' ast mem =
    let (instructions, _) = translateAST ast mem
     in (instructions, length instructions)

------- Lists

associateTypes :: Ast -> Memory -> Maybe Value
associateTypes (AstInt n) _ = Just (N (fromIntegral n))
associateTypes (AstBool b) _ = Just (B b)
associateTypes (AstString s) _ = Just (S s)
associateTypes (AstDouble d) _ = Just (D d)
associateTypes (AstChar c) _ = Just (C c)
associateTypes (AstList list) mem = Just (L (translateList list mem))
associateTypes (AstVar var) mem = case readMemory mem var of
    Just val -> associateTypes val mem
    _ -> Nothing
associateTypes _ _ = Nothing

translateList :: [Ast] -> Memory -> [Value]
translateList [] _ = []
translateList (x : xs) mem = case associateTypes x mem of
    Just val -> val : translateList xs mem
    _ -> []

translateMultIndexes :: [Int] -> Memory -> [Instruction]
translateMultIndexes (x : xs) mem =
    fst (translateAST (AstInt x) mem) ++ [call Nothing "get"] ++ translateMultIndexes xs mem
translateMultIndexes [] _ = []

------- Loops

addLoopFunction :: String -> Ast -> Ast -> Memory -> Either String Memory
addLoopFunction loopName cond block mem =
    addMemory mem loopName (AstDefineLoop loopName cond block)

translateLoopArg :: Ast -> Maybe Int -> Memory -> [Instruction]
translateLoopArg (AstVar _) (Just n) _ = [pushArg Nothing (n + 1)]
translateLoopArg (AstVar varName) Nothing mem = fst $ translateAST (AstVar varName) mem
translateLoopArg (AstBinaryFunc _ left right) n mem =
    translateLoopArg left n mem ++ translateLoopArg right n mem
translateLoopArg (AstListElem _ _) (Just n) _ = [pushArg Nothing (n + 1)]
translateLoopArg (AstListElem var idxs) Nothing mem = fst $ translateAST (AstListElem var idxs) mem
translateLoopArg _ Nothing _ = []
translateLoopArg ast _ mem = fst $ translateAST ast mem

------- Functions

translateBuiltin :: String -> [Ast] -> Memory -> ([Instruction], Memory)
translateBuiltin n' args mem =
    ( fst (translateArgs args mem 0 False) ++ [call Nothing n'],
      mem
    )

-------

translateAST :: Ast -> Memory -> ([Instruction], Memory)
translateAST (AstDefineVar (Variable varName _ varValue)) mem =
    ([], updateMemory mem varName varValue)
translateAST (AstVar varName) mem = (callArgs (AstVar varName) mem, mem)
translateAST (AstDefineFunc (Function _ funcArgs funcBody _)) mem =
    let newMem = freeMemory mem
     in (fst (translateArgs funcArgs newMem 0 True) ++ fst (translateToASM funcBody newMem), newMem)
translateAST (AstFunc (Function funcName funcArgs _ _)) mem
    | isBuiltin funcName = translateBuiltin funcName funcArgs mem
    | otherwise =
        ( fst (translateArgs funcArgs mem 0 False) ++ [call Nothing ('.' : funcName)],
          mem
        )
translateAST (AstReturn ast) mem = (fst (translateAST ast mem) ++ [ret Nothing], mem)
translateAST (AstPrefixFunc (op : _) ast) mem = handleAssignment ast (AstBinaryFunc [op] ast (AstInt 1)) mem -- check differences
translateAST (AstPostfixFunc (op : _) ast) mem = handleAssignment ast (AstBinaryFunc [op] ast (AstInt 1)) mem
translateAST (AstBinaryFunc "=" left right) mem = handleAssignment left right mem
translateAST (AstBinaryFunc op left right) mem = translateBinaryFunc op left right mem
translateAST (AstTernary cond doBlock elseBlock) mem = translateAST (AstIf cond doBlock [] (Just elseBlock)) mem
translateAST (AstIf cond ifBlock elseifEles elseEle) mem =
    let elseName = generateUniqueElseName mem
        newMemResult = addMemory mem elseName (AstIf cond ifBlock elseifEles elseEle)
     in case newMemResult of
            Right newMem ->
                let (condInstructions, memAfterCond) = translateConditionBlock cond ifBlock newMem elseName
                    (elseifInstructions, memAfterIfElse) = translateAllConditions elseifEles memAfterCond elseName
                    (elseInstructions, memAfterElse) = maybe ([], mem) (`translateAST` memAfterIfElse) elseEle
                 in (condInstructions ++ elseifInstructions ++ elseInstructions ++ [noop (Just $ "." ++ elseName)], memAfterElse)
            _ -> ([], mem)
translateAST (AstDefineLoop loopName cond block) mem =
    let (bodyBlock, updatedMem) = translateAST block mem
     in (translateLoopArg cond (Just (-1)) mem ++ fst (translateAST cond mem) ++ [jump Nothing (Right ("end" ++ loopName))] ++ bodyBlock ++ [jump Nothing (Right loopName)], updatedMem)
-- case translateAST block mem of
--     (bodyBlock, updatedMem) ->
--         (translateLoopArg cond (Just (-1)) mem ++ fst (translateAST cond mem) ++ [jump Nothing (Right ("end" ++ loopName))] ++ bodyBlock ++ [jump Nothing (Right loopName)], updatedMem)
--     _ -> ([], mem)
translateAST (AstLoop cond block) mem =
    trace ("loop: {cond (" ++ show cond ++ ") block (" ++ show block ++ ")") $
        let loopName = generateUniqueLoopName mem
            newMemResult = addLoopFunction loopName cond block mem
         in case newMemResult of
                Right updatedMem ->
                    (translateLoopArg cond Nothing mem ++ [call Nothing ("." ++ loopName), noop (Just $ ".end" ++ loopName)], updatedMem)
                _ -> ([], mem)
translateAST (AstBlock block) mem = translateToASM block mem
-- translateAST AstBreak mem =
translateAST AstVoid mem = ([], mem)
translateAST (AstInt n) mem = ([push Nothing (N (fromIntegral n))], mem)
translateAST (AstBool b) mem = ([push Nothing (B b)], mem)
translateAST (AstString s) mem = ([push Nothing (S s)], mem)
translateAST (AstDouble d) mem = ([push Nothing (D d)], mem)
translateAST (AstChar c) mem = ([push Nothing (C c)], mem)
translateAST (AstList list) mem = ([push Nothing (L (translateList list mem))], mem)
translateAST (AstListElem var idxs) mem =
    (fst (translateAST (AstVar var) mem) ++ translateMultIndexes idxs mem, mem)
translateAST _ mem = ([], mem)

translateToASM :: [Ast] -> Memory -> ([Instruction], Memory)
translateToASM asts mem = foldl processAST ([], mem) asts
  where
    processAST :: ([Instruction], Memory) -> Ast -> ([Instruction], Memory)
    processAST (instructions, currentMem) ast =
        let (newInstructions, updatedMem) = translateAST ast currentMem
         in (instructions ++ newInstructions, updatedMem)
