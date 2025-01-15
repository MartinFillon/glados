{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Functions
-}

module Compiler.Translation.Functions (isBuiltin, isSingleOp, translateOpInst) where

import VirtualMachine.Instructions (Instruction (..), call, noop)
import VirtualMachine.Operators (operators)

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
translateOpInst "|" = call Nothing "bor"
translateOpInst "&" = call Nothing "band"
translateOpInst "^" = call Nothing "xor"
translateOpInst ">>" = call Nothing "shiftR"
translateOpInst "<<" = call Nothing "shiftL"
-- ">=" =
-- "<=" =
translateOpInst _ = noop Nothing

isSingleOp :: String -> Bool
isSingleOp op = op `notElem` ["+=", "-=", "*=", "%=", "/=", "|=", "&=", "^=", ">>=", "<<="]

isBuiltin :: String -> Bool
isBuiltin s = any (\(n, _) -> n == s) operators
