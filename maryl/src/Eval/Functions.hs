{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Functions
-}

module Eval.Functions (checkBuiltins, evalArgs) where

import Compiler.Translation.Functions (isBuiltin)
import Memory (Memory, updateMemory)
import Parsing.ParserAst (Ast (..), Variable (..))

checkBuiltins :: String -> Ast -> Memory -> Either String (Ast, Memory)
checkBuiltins func ast mem
    | isBuiltin func = Right (ast, mem) -- add some error handling?
    | otherwise = Left ("Function \"" ++ func ++ "\" isn't defined.")

evalArgs :: [Ast] -> Memory -> Memory
evalArgs [] mem = mem
evalArgs ((AstDefineVar (Variable varName varType varValue)) : xs) mem =
    evalArgs xs (updateMemory mem varName (AstArg (AstDefineVar (Variable varName varType varValue)) Nothing))
evalArgs _ mem = mem
