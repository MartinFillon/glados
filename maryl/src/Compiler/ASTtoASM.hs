{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ASTtoASM
-}

module Compiler.ASTtoASM () where

import Parsing.ParserAst (Ast (..), Function(..), Variable(..))
import VirtualMachine.Instructions (Inst(..), Value(..), Instruction(..), Label(..), call, push, ret)
import Memory (Memory)

-- translateToASM :: [Ast] -> Memory -> [Instruction]
-- translateToASM asts mem = concatMap (translateAST mem) asts

-- translateAST :: Memory -> Ast -> [Instruction]
-- translateAST mem (AstDefineVar (Variable name _ value)) =
--     case value of
--         AstInt n -> [push Nothing (N n), call Nothing ("push " ++ name)]
--         AstBool b -> [push Nothing (B b), call Nothing ("push " ++ name)]
--         AstString s -> [push Nothing (S s), call Nothing ("push " ++ name)]
--         _ -> error "Unsupported variable type"
-- -- translateAST mem (AstDefineFunc (Function name args body _ )) =
-- --     let bodyInstructions = concatMap (translateAST mem) body
-- --     in [Label name] ++ bodyInstructions ++ [ret]
-- translateAST mem (AstBinaryFunc op left right) =
--     let leftInstructions = translateAST mem left
--         rightInstructions = translateAST mem right
--         opInstruction = case op of
--             "+" -> call Nothing "add"
--             "-" -> call Nothing "sub"
--             "*" -> call Nothing "mul"
--             "/" -> call Nothing "div"
--             _   -> error ("Unsupported binary operator: " ++ op)
--     in leftInstructions ++ rightInstructions ++ [opInstruction]
-- translateAST _ _ = error "Unsupported AST node"
