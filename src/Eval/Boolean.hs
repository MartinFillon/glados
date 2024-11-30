{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Boolean
-}

module Eval.Boolean (
    evalAnd,
    evalOr,
    evalNot,
    evalEq,
    evalLt,
    evalGt,
) where

import Parsing.SExprToAst (Ast (..))

booleanOperator :: (Bool -> Bool -> Bool) -> [Ast] -> Maybe Ast
booleanOperator op [AstBool b1, AstBool b2] = Just (AstBool (b1 `op` b2))
booleanOperator op [AstBool b1, AstInt i] = Just (AstBool (b1 `op` (i /= 0)))
booleanOperator op [AstInt i, AstBool b1] = Just (AstBool (b1 `op` (i /= 0)))
booleanOperator op [AstBool b1, AstFloat f] = Just (AstBool (b1 `op` (f /= 0)))
booleanOperator op [AstFloat f, AstBool b1] = Just (AstBool (b1 `op` (f /= 0)))
booleanOperator op [AstInt i1, AstInt i2] = Just (AstBool ((i1 /= 0) `op` (i2 /= 0)))
booleanOperator op [AstFloat f1, AstFloat f2] = Just (AstBool ((f1 /= 0) `op` (f2 /= 0)))
booleanOperator op [AstInt i, AstFloat f] = Just (AstBool ((i /= 0) `op` (f /= 0)))
booleanOperator op [AstFloat f, AstInt i] = Just (AstBool ((f /= 0) `op` (i /= 0)))
booleanOperator _ _ = Nothing

evalAnd :: [Ast] -> Maybe Ast
evalAnd = booleanOperator (&&)

evalOr :: [Ast] -> Maybe Ast
evalOr = booleanOperator (||)

evalNot :: [Ast] -> Maybe Ast
evalNot [AstBool a] = Just $ AstBool (not a)
evalNot _ = Nothing

evalEq :: [Ast] -> Maybe Ast
evalEq = booleanOperator (==)

evalLt :: [Ast] -> Maybe Ast
evalLt = booleanOperator (<)

evalGt :: [Ast] -> Maybe Ast
evalGt = booleanOperator (>)
