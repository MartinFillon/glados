module Eval.Boolean (
    evalAnd,
    evalOr,
    evalNot,
    evalEq,
    evalLt,
    evalGt,
) where

import Parsing.SExprToAst (Ast (..))

evalAnd :: [Ast] -> Either String Ast
evalAnd [AstBool b1, AstBool b2] = Right (AstBool (b1 && b2))
evalAnd _ = Left "Invalid arguments for `and`"

evalOr :: [Ast] -> Either String Ast
evalOr [AstBool b1, AstBool b2] = Right (AstBool (b1 || b2))
evalOr _ = Left "Invalid arguments for `or`"

evalNot :: [Ast] -> Either String Ast
evalNot [AstBool b] = Right (AstBool (not b))
evalNot _ = Left "Invalid arguments for `not`"

evalEq :: [Ast] -> Either String Ast
evalEq [AstInt i1, AstInt i2] = Right (AstBool (i1 == i2))
evalEq [AstFloat f1, AstFloat f2] = Right (AstBool (f1 == f2))
evalEq [AstInt i, AstFloat f] = Right (AstBool (fromIntegral i == f))
evalEq [AstFloat f, AstInt i] = Right (AstBool (f == fromIntegral i))
evalEq [AstBool b1, AstBool b2] = Right (AstBool (b1 == b2))
evalEq _ = Left "Invalid arguments for equality"

evalLt :: [Ast] -> Either String Ast
evalLt [AstInt i1, AstInt i2] = Right (AstBool (i1 < i2))
evalLt [AstFloat f1, AstFloat f2] = Right (AstBool (f1 < f2))
evalLt _ = Left "Invalid arguments for less-than"

evalGt :: [Ast] -> Either String Ast
evalGt [AstInt i1, AstInt i2] = Right (AstBool (i1 > i2))
evalGt [AstFloat f1, AstFloat f2] = Right (AstBool (f1 > f2))
evalGt _ = Left "Invalid arguments for greater-than"
