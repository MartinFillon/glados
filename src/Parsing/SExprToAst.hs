{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- SExprToAst
-}
{-# LANGUAGE InstanceSigs #-}

module Parsing.SExprToAst (
    printTree,
    sexprToAST,
    Ast (..),
    Function (..),
) where

import Parsing.ParserSExpr (Atom (..), Sexpr (..))

data Function = Function
    { name :: String,
      args :: [Ast]
    }

data Ast
    = Define String Ast
    | Call Function -- function param1 param2
    | AstInt Int
    | AstFloat Double
    | AstBool Bool
    | AstSymbol String (Maybe Ast) -- name value
    | Lambda [String] Ast
    | Apply Ast [Ast]

instance Show Ast where
    show :: Ast -> String
    show (AstInt i) = show i
    show (AstFloat f) = show f
    show (AstBool b) = if b then "#t" else "#f"
    show (AstSymbol n s) = '(' : n ++ " = " ++ show s ++ ")"
    show (Define a b) = "Define " ++ show a ++ " = " ++ show b
    show (Call (Function n a)) = "Call " ++ n ++ concatMap (\x -> ' ' : show x) a
    show (Apply ast lAst) = show ast ++ ": " ++ concatMap (\x -> ' ' : show x) lAst
    show (Lambda _ _) = "Lambda"

instance Eq Ast where
    (==) :: Ast -> Ast -> Bool
    (AstInt i1) == (AstInt i2) = i1 == i2
    (AstBool b1) == (AstBool b2) = b1 == b2
    (AstSymbol n1 s1) == (AstSymbol n2 s2) = n1 == n2 && s1 == s2
    (Define n1 a1) == (Define n2 a2) = n1 == n2 && a1 == a2
    (Call (Function n1 a1)) == (Call (Function n2 a2)) = n1 == n2 && a1 == a2
    (Lambda p1 b1) == (Lambda p2 b2) = p1 == p2 && b1 == b2
    _ == _ = False

getSymbol :: Sexpr Int Double -> Maybe String
getSymbol (Atom (String s)) = Just s
getSymbol _ = Nothing

{-getInteger :: SExpr -> Maybe Int
getInteger (Integer i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List l) = Just l
getList _ = Nothing-}

printTree :: Sexpr Int Double -> Maybe String
printTree (Atom (String s)) = Just ("a " ++ show s)
printTree (Atom (Number i)) = Just ("an " ++ show i)
printTree (Atom (Float f)) = Just ("a " ++ show f)
printTree (Atom (Bool True)) = Just "Bool: #t"
printTree (Atom (Bool False)) = Just "Bool: #f"
printTree (List [x]) = printTree x
printTree (List (x : xs)) =
    (\a b -> a ++ ", " ++ b)
        <$> printTree x
        <*> printTree (List xs)
printTree (List []) = Nothing

------------ Sexpr -> AST

sexprToAST :: Sexpr Int Double -> Maybe Ast
sexprToAST (Atom (Number i)) = Just (AstInt i)
sexprToAST (Atom (Bool b)) = Just (AstBool b)
sexprToAST (Atom (Float f)) = Just (AstFloat f)
sexprToAST (Atom (String s)) = Just (AstSymbol s Nothing)
sexprToAST (List [Atom (String "define"), Atom (String s), expr]) =
    Define s <$> sexprToAST expr
-- Adjust sexprToAST to handle lambda expressions
sexprToAST (List [Atom (String "lambda"), List params, body]) =
    (\paramNames bodyAst -> Lambda paramNames bodyAst) <$> mapM getSymbol params <*> sexprToAST body
sexprToAST (List ((Atom (String s)) : a)) = mapM sexprToAST a >>= \argAsts -> Just (Call (Function s argAsts))
-- Adjust sexprToAST to handle function applications
sexprToAST (List (func : a)) = (\x y -> Apply x y) <$> sexprToAST func <*> mapM sexprToAST a
sexprToAST _ = Nothing

-----------
