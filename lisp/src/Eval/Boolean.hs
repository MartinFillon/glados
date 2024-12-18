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

import Data.Data (typeOf)
import Memory (Memory)
import Parsing.SExprToAst (Ast (..))
import Eval.Maths (isNumeric)

evalAnd :: Memory -> [Ast] -> Either String (Ast, Memory)
evalAnd mem [AstBool b1, AstBool b2] = Right (AstBool (b1 && b2), mem)
evalAnd _ [a, b]
    | typeOf a == typeOf AstBool =
        Left ("Arguments " ++ show b ++ " out of bound for `and`")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for `and`")
evalAnd _ _ = Left "Invalid arguments for `and`"

evalOr :: Memory -> [Ast] -> Either String (Ast, Memory)
evalOr mem [AstBool b1, AstBool b2] = Right (AstBool (b1 || b2), mem)
evalOr _ [a, b]
    | typeOf a == typeOf AstBool =
        Left ("Arguments " ++ show b ++ " out of bound for `or`")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for `or`")
evalOr _ _ = Left "Invalid arguments for `or`"

evalNot :: Memory -> [Ast] -> Either String (Ast, Memory)
evalNot mem [AstBool b] = Right (AstBool (not b), mem)
evalNot _ b = Left ("Invalid arguments " ++ show b ++ " for `not`")


isAST :: Ast -> Bool
isAST (AstBool _) = True
isAST (AstInt _) = True
isAST (AstFloat _) = True
isAST _ = False

evalEq :: Memory -> [Ast] -> Either String (Ast, Memory)
evalEq mem [AstInt i1, AstInt i2] = Right (AstBool (i1 == i2), mem)
evalEq mem [AstFloat f1, AstFloat f2] = Right (AstBool (f1 == f2), mem)
evalEq mem [AstInt i, AstFloat f] = Right (AstBool (fromIntegral i == f), mem)
evalEq mem [AstFloat f, AstInt i] = Right (AstBool (f == fromIntegral i), mem)
evalEq mem [AstBool b1, AstBool b2] = Right (AstBool (b1 == b2), mem)
evalEq _ [a, b]
    | isAST a =
        Left ("Arguments " ++ show b ++ " out of bound for `eq?`")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for `eq?`")
evalEq _ _ = Left "Invalid arguments for equality"

evalLt :: Memory -> [Ast] -> Either String (Ast, Memory)
evalLt mem [AstInt i1, AstInt i2] = Right (AstBool (i1 < i2), mem)
evalLt mem [AstFloat f1, AstFloat f2] = Right (AstBool (f1 < f2), mem)
evalLt _ [a, b]
    | isNumeric a =
        Left ("Arguments " ++ show b ++ " out of bound for `<`")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for `<`")
evalLt _ _ = Left "Invalid arguments for less-than"

evalGt :: Memory -> [Ast] -> Either String (Ast, Memory)
evalGt mem [AstInt i1, AstInt i2] = Right (AstBool (i1 > i2), mem)
evalGt mem [AstFloat f1, AstFloat f2] = Right (AstBool (f1 > f2), mem)
evalGt _ [a, b]
    | isNumeric a =
        Left ("Arguments " ++ show b ++ " out of bound for `>`")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for `>`")
evalGt _ _ = Left "Invalid arguments for greater-than"
