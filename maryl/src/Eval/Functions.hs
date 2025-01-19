{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Functions
-}

module Eval.Functions (checkBuiltins, evalArgs, furtherEvalFunc) where

import Compiler.Translation.Functions (isBuiltin)
import Memory (Memory, addMemory)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Variable (..))

checkBuiltins :: String -> Ast -> Memory -> Either String (Ast, Memory)
checkBuiltins func ast mem
    | isBuiltin func = Right (ast, mem) -- !! add some error handling
    | otherwise = Left ("Function \"" ++ func ++ "\" isn't defined.")

-- | Evaluate valid argument definition in a function.
evalArgs :: [Ast] -> Memory -> Either String Memory
evalArgs [] mem = Right mem
evalArgs ((AstDefineVar (Variable var varType val)) : xs) mem =
    either
        (\err -> Left ("Argument " ++ var ++ " isn't valid, " ++ err ++ "."))
        (evalArgs xs)
        (addMemory mem var (AstArg (AstDefineVar (Variable var varType val)) Nothing))
evalArgs _ mem = Right mem

-- | Evaluate further function definition for builtin option
furtherEvalFunc :: Ast -> MarylType -> Memory -> Either String Ast
furtherEvalFunc (AstFunc func@(Function funcName _ _ newReturn)) expectedType _
    | expectedType == newReturn = Right (AstFunc func)
    | otherwise =
        if isBuiltin funcName
            then Right (AstFunc func)
            else
                Left
                (  "Call to function "
                    ++ fName func
                    ++ " is invalid, function returns "
                    ++ show newReturn
                    ++ " but expecting "
                    ++ show expectedType
                    ++ "."
                )
furtherEvalFunc ast expectedType _ =
    Left ("Call to function " ++ show ast ++ " is invalid, expecting " ++ show expectedType ++ " as return value.")
