{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Functions
-}

module Compiler.Translation.Functions (isBuiltin, isSingleOp, pushArgs, translateOpInst) where

import Memory (Memory, updateMemory)
import Parsing.ParserAst (Ast (..), Variable (..))
import VirtualMachine.Instructions (Instruction (..), call, noop)
import VirtualMachine.Operators (operators)

-- | Translates an operator to its corresponding instruction call.
translateOpInst :: String -> Instruction
translateOpInst "*" = call Nothing "mul"
translateOpInst "**" = call Nothing "pow"
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
translateOpInst "|" = call Nothing "bor"
translateOpInst "&" = call Nothing "band"
translateOpInst "^" = call Nothing "xor"
translateOpInst ">>" = call Nothing "shiftR"
translateOpInst "<<" = call Nothing "shiftL"
-- translateOpInst "**" = call Nothing "pow" ?
translateOpInst _ = noop Nothing

-- | Takes an operator and evaluates it as a simple operator, as opposed to a compound assignment operator.
isSingleOp :: String -> Bool
isSingleOp op =
    op
        `notElem` [ "+=",
                    "-=",
                    "*=",
                    "%=",
                    "/=",
                    "|=",
                    "&=",
                    "^=",
                    ">>=",
                    "<<="
                  ]

-- | Checks if the VM has a function of the same name to associate the call.
isBuiltin :: String -> Bool
isBuiltin s = any (\(n, _) -> n == s) operators

-- | Identifies any function argument and wraps it in a 'AstArg'.
pushArgs :: [Ast] -> Memory -> Int -> Memory
pushArgs [] mem _ = mem
pushArgs (AstDefineVar (Variable varName varType varValue) : xs) mem idx =
    let updatedMem =
            updateMemory
                mem
                varName
                (AstArg (AstDefineVar (Variable varName varType varValue)) (Just idx))
     in pushArgs xs updatedMem (idx + 1)
pushArgs _ mem _ = mem
