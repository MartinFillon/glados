{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.ASTtoASM (translateToASM, translateAST) where

import Debug.Trace (trace)
import Memory (Memory (..), readMemory, freeMemory, updateMemory)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, push, pushArg, ret)

translateToASM :: [Ast] -> Memory -> [Instruction]
translateToASM asts mem = fst $ foldl processAST ([], mem) asts
  where
    processAST (instructions, currentMem) ast =
        let (newInstructions, updatedMem) = translateAST ast currentMem
         in (instructions ++ newInstructions, updatedMem)

callArgs :: Ast -> Memory -> [Instruction]
callArgs (AstVar vName) mem = 
    case readMemory mem vName of
        Just ast -> fst $ translateAST ast mem
        Nothing -> []
callArgs ast mem = fst $ translateAST ast mem

translateArgs :: [Ast] -> Memory -> Int -> Bool -> ([Instruction], Memory)
translateArgs [] mem _ _ = ([], mem)
translateArgs (x : xs) mem n False = (callArgs x mem ++ fst (translateArgs xs mem n False), mem) -- called args
translateArgs (_ : xs) mem n True = (pushArg Nothing n : fst (translateArgs xs mem (n + 1) True), mem) -- defined args (pusharg)

translateAST :: Ast -> Memory -> ([Instruction], Memory)
translateAST (AstDefineVar (Variable vName vType vValue )) mem =
    ([], updateMemory mem vName vValue)
translateAST (AstVar vName) mem = (callArgs (AstVar vName) mem, mem)
translateAST (AstDefineFunc (Function fName fArgs fBody _)) mem =
    let
        newMem = freeMemory mem
        (argInstructions, memAfterArgs) = translateArgs fArgs newMem 0 True
        bodyInstructions = translateToASM fBody memAfterArgs
        finalInstructions = argInstructions ++ bodyInstructions ++ [ret Nothing]
    in
        (finalInstructions, memAfterArgs)
translateAST (AstFunc (Function fName fArgs _ fType)) mem = (fst (translateArgs fArgs mem 0 False) ++ [call Nothing ("." ++ fName)], mem)
translateAST (AstReturn ast) mem = translateAST ast mem
translateAST (AstBinaryFunc op left right) mem =
    let (leftInstructions, _) = translateAST left mem 
        (rightInstructions, _) = translateAST right mem
        opInstruction = case op of
            -- "++" ->
            "*" -> call Nothing "mul"
            "/" -> call Nothing "div"
            "%" -> call Nothing "mod"
            "+" -> call Nothing "add"
            "-" -> call Nothing "sub"
            "|" -> call Nothing "or"
            "&" -> call Nothing "and"
            "==" -> call Nothing "eq" -- tocheck
            -- "!=" ->
            ">" -> call Nothing "greater" -- tocheck
            -- ">=" ->
            "<" -> call Nothing "less" -- tocheck
            -- "<=" ->
            -- "||" ->
            -- "&&" ->
            -- "=" -> trace "yo" $ call Nothing "set" --doing next
            _ -> error ("Unsupported binary operator: " ++ op)
     in (leftInstructions ++ rightInstructions ++ [opInstruction], mem)
translateAST (AstIf cond block elseifEle elseEle) mem = (fst (translateAST cond mem) ++ fst (translateAST block mem), mem)
translateAST AstVoid mem = ([], mem)
translateAST (AstInt n) mem = ([push Nothing (N (fromIntegral n))], mem)
translateAST (AstBool b) mem = ([push Nothing (B b)], mem)
translateAST (AstString s) mem = ([push Nothing (S s)], mem)
translateAST (AstDouble d) mem = ([push Nothing (D d)], mem)
translateAST (AstChar c) mem = ([push Nothing (S (c : ""))], mem)
-- list
translateAST _ mem = ([], mem)
