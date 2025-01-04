{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.ASTtoASM (translateToASM, translateAST) where

import Debug.Trace (trace)
import Parsing.ParserAst (Ast (..), Function (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), Value (..), call, push, pushArg, ret)

translateToASM :: [Ast] -> [Instruction]
translateToASM = concatMap translateAST

translateArgs :: [Ast] -> Int -> [Instruction]
translateArgs [] _ = []
translateArgs (_ : xs) n = pushArg Nothing n : translateArgs xs (n + 1)

translateAST :: Ast -> [Instruction]
translateAST (AstDefineVar (Variable _ _ value)) =
    case value of
        AstInt n -> [push Nothing (N (fromIntegral n))]
        AstBool b -> [push Nothing (B b)]
        AstString s -> [push Nothing (S s)]
        AstDouble d -> [push Nothing (D d)]
        AstChar c -> [push Nothing (S (c : ""))]
        -- list
        -- void ??
        _ -> error "Unsupported variable type"
translateAST (AstDefineFunc (Function _ args body _)) =
    translateArgs args 0 ++ translateToASM body
translateAST (AstBinaryFunc op left right) =
    let leftInstructions = translateAST left
        rightInstructions = translateAST right
        opInstruction = case op of
            -- "++" ->
            "*" -> call Nothing "mul"
            "/" -> call Nothing "div"
            "%" -> call Nothing "mod"
            "+" -> call Nothing "add"
            "-" -> call Nothing "sub"
            "|" -> call Nothing "or"
            "&" -> call Nothing "and"
            "==" -> call Nothing "eq"
            -- "!=" ->
            ">" -> call Nothing "greater"
            -- ">=" ->
            "<" -> call Nothing "less"
            -- "<=" ->
            -- "||" ->
            -- "&&" ->
            "=" -> call Nothing "set"
            _ -> error ("Unsupported binary operator: " ++ op)
     in leftInstructions ++ rightInstructions ++ [opInstruction]
translateAST (AstReturn ast) = translateAST ast ++ [ret Nothing]
translateAST (AstVar _) = []
translateAST rest = []
