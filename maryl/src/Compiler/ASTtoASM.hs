{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.ASTtoASM (translateToASM, translateAST) where

import Compiler.Streamline (clarifyAST, updateList)
import Debug.Trace (trace)
import Memory (Memory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, jumpf, noop, push, pushArg, ret)

translateToASM :: [Ast] -> Memory -> [Instruction]
translateToASM asts mem = fst $ foldl processAST ([], mem) asts
  where
    processAST (instructions, currentMem) ast =
        let (newInstructions, updatedMem) = translateAST ast currentMem
         in (instructions ++ newInstructions, updatedMem)

-- (= assignment operator)
handleAssignment :: Ast -> Ast -> Memory -> ([Instruction], Memory)
handleAssignment (AstVar var) right mem =
    let newMem = updateMemory mem var (clarifyAST right mem)
     in ([], newMem)
handleAssignment (AstListElem var (x : xs)) right mem =
    let (clarified, _) = updateList var (AstListElem var (x : xs)) mem (clarifyAST right mem)
        newMem = updateMemory mem var clarified
     in (fst (translateAST (AstVar var) mem) ++ fst (translateAST (AstInt x) mem) ++ fst (translateAST right mem) ++ [call Nothing "set"], newMem)
handleAssignment _ _ mem = ([], mem)

-- (call defined Vars)
callArgs :: Ast -> Memory -> [Instruction]
callArgs (AstVar varName) mem =
    case readMemory mem varName of
        Just ast -> fst $ translateAST ast mem
        Nothing -> []
callArgs ast mem = fst $ translateAST ast mem

-- (parsing AstVar)
translateArgs :: [Ast] -> Memory -> Int -> Bool -> ([Instruction], Memory)
translateArgs [] mem _ _ = ([], mem)
translateArgs (x : xs) mem n False =
    (callArgs x mem ++ fst (translateArgs xs mem n False), mem) -- called args
translateArgs (_ : xs) mem n True =
    (pushArg Nothing n : fst (translateArgs xs mem (n + 1) True), mem) -- defined args (pusharg)

-- (blocks in scope, length for jump/jumpf)
translateBlock' :: Ast -> Memory -> ([Instruction], Int)
translateBlock' (AstBlock block) mem =
    let instructions = translateToASM block mem
     in (instructions, length instructions)
translateBlock' _ _ = ([], 0)

translateCondBlock' :: Ast -> Ast -> Memory -> ([Instruction], Memory)
translateCondBlock' cond block mem =
    let (condInstructions, memAfterCond) = translateAST cond mem -- if/else if
        (blockInstructions, blockLength) = translateBlock' block memAfterCond -- cond
        allInstructions = condInstructions ++ [jumpf Nothing (Left blockLength)] ++ blockInstructions
     in (allInstructions, mem)

-- (if/elseif cond with blocks)
translateCondBlock :: [Ast] -> Memory -> ([Instruction], Memory)
translateCondBlock (x : xs) mem =
    let (topTranslated, newTopMem) = translateAST x mem
        (restTranslated, finalMem) = translateCondBlock xs newTopMem
     in (topTranslated ++ restTranslated, finalMem)
translateCondBlock [] mem = ([], mem)

-- (optional block (`else`))
translateOptionalBlock :: Maybe Ast -> Memory -> ([Instruction], Memory)
translateOptionalBlock (Just ast) mem = translateAST ast mem
translateOptionalBlock Nothing mem = ([], mem)

translateOpInst :: String -> Instruction
translateOpInst "*" = call Nothing "mul"
translateOpInst "/" = call Nothing "div"
translateOpInst "%" = call Nothing "mod"
translateOpInst "+" = call Nothing "add"
translateOpInst "-" = call Nothing "sub"
translateOpInst "or" = call Nothing "or"
translateOpInst "and" = call Nothing "and"
translateOpInst "==" = call Nothing "eq"
translateOpInst "!=" = call Nothing "neq"
translateOpInst ">" = call Nothing "greater"
translateOpInst "<" = call Nothing "less"
-- ">=" =
-- "<=" =
-- "||" = to check with alexandre
-- "&&" = to check with alexandre
translateOpInst _ = noop Nothing

isSingleOp :: String -> Bool
isSingleOp "+=" = False
isSingleOp "-=" = False
isSingleOp "*=" = False
isSingleOp "/=" = False
isSingleOp "&=" = False
isSingleOp "^=" = False
isSingleOp ">>=" = False
isSingleOp "<<=" = False
isSingleOp _ = True

updateAssignment :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
updateAssignment "+=" left right mem = handleAssignment left (AstBinaryFunc "+" (clarifyAST left mem) right) mem
updateAssignment "-=" left right mem = handleAssignment left (AstBinaryFunc "-" (clarifyAST left mem) right) mem
updateAssignment "*=" left right mem = handleAssignment left (AstBinaryFunc "*" (clarifyAST left mem) right) mem
updateAssignment "/=" left right mem = handleAssignment left (AstBinaryFunc "/" (clarifyAST left mem) right) mem
-- updateAssignment "&=" left right mem =
-- updateAssignment "^=" left right mem =
-- updateAssignment ">>=" left right mem =
-- updateAssignment "<<=" left right mem =
updateAssignment _ _ _ mem = ([], mem)

translateBinaryFunc :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
translateBinaryFunc op left right mem
    | isSingleOp op = (fst (translateAST left mem) ++ fst (translateAST right mem) ++ [translateOpInst op], mem)
    | otherwise = updateAssignment op left right mem

associateTypes :: Ast -> Memory -> Maybe Value
associateTypes (AstInt n) _ = Just (N (fromIntegral n))
associateTypes (AstBool b) _ = Just (B b)
associateTypes (AstString s) _ = Just (S s)
associateTypes (AstDouble d) _ = Just (D d)
associateTypes (AstChar c) _ = Just (S (c : ""))
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

translateAST :: Ast -> Memory -> ([Instruction], Memory)
translateAST (AstDefineVar (Variable varName _ varValue)) mem =
    ([], updateMemory mem varName varValue)
translateAST (AstVar varName) mem = (callArgs (AstVar varName) mem, mem)
translateAST (AstDefineFunc (Function _ funcArgs funcBody _)) mem =
    let newMem = freeMemory mem
     in (fst (translateArgs funcArgs newMem 0 True) ++ translateToASM funcBody newMem, newMem)
translateAST (AstFunc (Function funcName funcArgs _ _)) mem =
    (fst (translateArgs funcArgs mem 0 False) ++ [call Nothing ("." ++ funcName)], mem)
translateAST (AstReturn ast) mem = (fst (translateAST ast mem) ++ [ret Nothing], mem)
translateAST (AstPrefixFunc "++" ast) mem = translateAST (AstBinaryFunc "=" ast (AstBinaryFunc "+" ast (AstInt 1))) mem -- check differences
translateAST (AstPrefixFunc "--" ast) mem = translateAST (AstBinaryFunc "=" ast (AstBinaryFunc "-" ast (AstInt 1))) mem
translateAST (AstPostfixFunc "++" ast) mem = translateAST (AstBinaryFunc "=" ast (AstBinaryFunc "+" ast (AstInt 1))) mem
translateAST (AstPostfixFunc "--" ast) mem = translateAST (AstBinaryFunc "=" ast (AstBinaryFunc "-" ast (AstInt 1))) mem
translateAST (AstBinaryFunc "=" left right) mem =
    handleAssignment left right mem
translateAST (AstBinaryFunc op left right) mem = translateBinaryFunc op left right mem
translateAST (AstTernary cond doBlock elseBlock) mem =
    let (condInstructions, memAfterCond) = translateCondBlock' cond doBlock mem
        (elseInstructions, memAfterElse) = translateAST elseBlock memAfterCond
     in (condInstructions ++ elseInstructions, memAfterElse)
translateAST (AstIf cond block elseifEles elseEle) mem =
    let (condInstructions, memAfterCond) = translateCondBlock' cond block mem
        (elseifInstructions, memAfterIfElse) = translateCondBlock elseifEles memAfterCond
        (elseInstructions, memAfterElse) = translateOptionalBlock elseEle memAfterIfElse
     in (condInstructions ++ elseifInstructions ++ elseInstructions, memAfterElse)
-- translateAst (AstLoop cond block) mem =
--     let (condInstructions, memAfterCond) = translateAst cond mem
--         (blockInstructions, blockLength) = translateBlock' block memAfterCond
--         allInstructions = condInstructions ++ blockInstructions ++ [jump ]
--      in (allInstructions, finalMem)
translateAST (AstBlock block) mem = (translateToASM block mem, mem)
translateAST AstVoid mem = ([], mem)
translateAST (AstInt n) mem = ([push Nothing (N (fromIntegral n))], mem)
translateAST (AstBool b) mem = ([push Nothing (B b)], mem)
translateAST (AstString s) mem = ([push Nothing (S s)], mem)
translateAST (AstDouble d) mem = ([push Nothing (D d)], mem)
translateAST (AstChar c) mem = ([push Nothing (S (c : ""))], mem)
translateAST (AstList list) mem = ([push Nothing (L (translateList list mem))], mem)
translateAST (AstListElem var idxs) mem =
    (fst (translateAST (AstVar var) mem) ++ translateMultIndexes idxs mem, mem)
translateAST _ mem = ([], mem)
