{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.ASTtoASM (translateToASM, translateAST) where

import Debug.Trace (trace)
import Eval.Evaluator (evalNode)
import Memory (Memory, freeMemory, readMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, jumpf, push, pushArg, ret)

translateToASM :: [Ast] -> Memory -> [Instruction]
translateToASM asts mem = fst $ foldl processAST ([], mem) asts
  where
    processAST (instructions, currentMem) ast =
        let (newInstructions, updatedMem) = translateAST ast currentMem
         in (instructions ++ newInstructions, updatedMem)

-- (evaluating updates on same var)
updateMemory' :: Memory -> String -> Ast -> ([Instruction], Memory) -- to fix
--     case evalNode mem (AstBinaryFunc op left right) of
--         Right (value, newMemory) -> updateMemory newMemory var value
--         _ -> error ("Failed to evaluate AST for assignment to " ++ var)
-- updateMemory' mem var (AstBinaryFunc op left right) = do
--     let (instructions, newMem) = translateAST (AstBinaryFunc op left right) mem
--     trace (show instructions) return ()
--     (instructions, newMem)
updateMemory' mem var value =
    let updatedMem = updateMemory mem var value
     in ([], updatedMem)

-- (= operator)
handleAssignment :: Ast -> Ast -> Memory -> ([Instruction], Memory)
handleAssignment (AstVar var) right mem =
    updateMemory' mem var right
handleAssignment _ _ mem = ([], mem)

-- (call defined vars)
callArgs :: Ast -> Memory -> [Instruction]
callArgs (AstVar varName) mem =
    case readMemory mem varName of
        Just ast -> fst $ translateAST ast mem
        Nothing -> []
callArgs ast mem = fst $ translateAST ast mem

-- (AstVar parsing)
translateArgs :: [Ast] -> Memory -> Int -> Bool -> ([Instruction], Memory)
translateArgs [] mem _ _ = ([], mem)
translateArgs (x : xs) mem n False =
    (callArgs x mem ++ fst (translateArgs xs mem n False), mem) -- called args
translateArgs (_ : xs) mem n True =
    (pushArg Nothing n : fst (translateArgs xs mem (n + 1) True), mem) -- defined args (pusharg)

-- (blocks in scope for length + no memory update)
translateBlock' :: Ast -> Memory -> ([Instruction], Int)
translateBlock' (AstBlock block) mem =
    let instructions = translateToASM block mem
     in (instructions, length instructions)
translateBlock' _ _ = ([], 0)

-- (optional blocks (`else`))
translateOptionalBlock :: Maybe Ast -> Memory -> ([Instruction], Memory)
translateOptionalBlock (Just ast) mem = translateAST ast mem
translateOptionalBlock Nothing mem = ([], mem)

translateOpInst :: String -> Instruction
translateOpInst "*" = call Nothing "mul"
translateOpInst "/" = call Nothing "div"
translateOpInst "%" = call Nothing "mod"
translateOpInst "+" = call Nothing "add"
translateOpInst "-" = call Nothing "sub"
translateOpInst "|" = call Nothing "or"
translateOpInst "&" = call Nothing "and"
translateOpInst "==" = call Nothing "eq"
-- "!=" add neq ?
translateOpInst ">" = call Nothing "greater"
translateOpInst "<" = call Nothing "less"
-- ">=" =
-- "<=" =
-- "||" = to check with alexandre
-- "&&" = to check with alexandre
-- "++" = to check with alexandre
translateOpInst op = error ("Unsupported binary operator: " ++ op) -- dk how else

translateBinaryFunc :: String -> Ast -> Ast -> Memory -> ([Instruction], Memory)
translateBinaryFunc op left right mem =
    (fst (translateAST left mem) ++ fst (translateAST right mem) ++ [translateOpInst op], mem)

translateAST :: Ast -> Memory -> ([Instruction], Memory)
translateAST (AstDefineVar (Variable varName _ varValue)) mem =
    ([], updateMemory mem varName varValue)
translateAST (AstVar varName) mem = (callArgs (AstVar varName) mem, mem)
translateAST (AstDefineFunc (Function _ funcArgs funcBody _)) mem =
    let newMem = freeMemory mem
        finalInstructions =
            fst (translateArgs funcArgs newMem 0 True)
                ++ translateToASM funcBody newMem
     in (finalInstructions, newMem)
translateAST (AstFunc (Function funcName funcArgs _ _)) mem =
    (fst (translateArgs funcArgs mem 0 False) ++ [call Nothing ("." ++ funcName)], mem)
translateAST (AstReturn ast) mem = (fst (translateAST ast mem) ++ [ret Nothing], mem)
translateAST (AstBinaryFunc "=" left right) mem =
    handleAssignment left right mem
translateAST (AstBinaryFunc op left right) mem = translateBinaryFunc op left right mem
translateAST (AstIf cond block _ elseEle) mem =
    let (condInstructions, memAfterCond) = translateAST cond mem -- if
        (blockInstructions, blockLength) = translateBlock' block memAfterCond -- cond
        -- elseif to do
        (elseInstructions, memAfterElse) = translateOptionalBlock elseEle memAfterCond -- else
        allInstructions = condInstructions ++ [jumpf Nothing (Left blockLength)] ++ blockInstructions ++ elseInstructions
     in (allInstructions, memAfterElse)
translateAST (AstBlock block) mem = (translateToASM block mem, mem) -- always in scope so no mem
translateAST AstVoid mem = ([], mem)
translateAST (AstInt n) mem = ([push Nothing (N (fromIntegral n))], mem)
translateAST (AstBool b) mem = ([push Nothing (B b)], mem)
translateAST (AstString s) mem = ([push Nothing (S s)], mem)
translateAST (AstDouble d) mem = ([push Nothing (D d)], mem)
translateAST (AstChar c) mem = ([push Nothing (S (c : ""))], mem)
-- list
translateAST _ mem = ([], mem)
