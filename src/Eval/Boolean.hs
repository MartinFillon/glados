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

evalAnd :: [Ast] -> Maybe Ast
evalAnd [AstBool b1, AstBool b2] = Just (AstBool ((&&) b1 b2))
evalAnd _ = Nothing

evalOr :: [Ast] -> Maybe Ast
evalOr [AstBool b1, AstBool b2] = Just (AstBool ((||) b1 b2))
evalOr _ = Nothing

evalNot :: [Ast] -> Maybe Ast
evalNot [AstBool a] = Just $ AstBool (not a)
evalNot _ = Nothing

evalEq :: [Ast] -> Maybe Ast
evalEq [AstInt i1, AstInt i2] = Just $ AstBool (i1 == i2)
evalEq [AstFloat f1, AstFloat f2] = Just $ AstBool (f1 == f2)
evalEq [AstInt i, AstFloat f] = Just $ AstBool (fromIntegral i == f)
evalEq [AstFloat f, AstInt i] = Just $ AstBool (f == fromIntegral i)
evalEq [AstBool b1, AstBool b2] = Just $ AstBool (b1 == b2)
evalEq [AstBool b, AstInt i] = Just $ AstBool (b == (i /= 0))
evalEq [AstInt i, AstBool b] = Just $ AstBool ((i /= 0) == b)
evalEq [AstBool b, AstFloat f] = Just $ AstBool (b == (f /= 0))
evalEq [AstFloat f, AstBool b] = Just $ AstBool ((f /= 0) == b)
evalEq _ = Nothing

evalLt :: [Ast] -> Maybe Ast
evalLt [AstInt i1, AstInt i2] = Just $ AstBool (i1 < i2)
evalLt [AstFloat f1, AstFloat f2] = Just $ AstBool (f1 < f2)
evalLt _ = Nothing

evalGt :: [Ast] -> Maybe Ast
evalGt [AstInt i1, AstInt i2] = Just $ AstBool (i1 > i2)
evalGt [AstFloat f1, AstFloat f2] = Just $ AstBool (f1 > f2)
evalGt _ = Nothing
