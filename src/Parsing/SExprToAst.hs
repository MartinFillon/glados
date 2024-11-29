{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- SExprToAst
-}

module Parsing.SExprToAst (
    sexprToAST,
    Ast (..),
    evalAST,
) where

import GHC.Float (double2Int, int2Double)
import Parsing.ParserSExpr (Atom (..), Sexpr (..))

data Function = Function
    { name :: String,
      args :: [Ast]
    }
    deriving (Show)

data Ast
    = Define String Ast
    | Call Function -- function param1 param2
    | Value (Atom Int Double)
    deriving (Show)

evalMathOperator :: (Double -> Double -> Double) -> [Ast] -> Maybe Ast
evalMathOperator f [Value (Float x), Value (Float y)] = Just (Value (Float (f x y)))
evalMathOperator f [Value (Number x), Value (Number y)] = Just (Value (Number (double2Int (f (int2Double x) (int2Double y)))))
evalMathOperator f [Value (Float x), Value (Number y)] = Just (Value (Float (f x (int2Double y))))
evalMathOperator f [Value (Number x), Value (Float y)] = Just (Value (Float (f (int2Double x) y)))
evalMathOperator _ _ = Nothing

evalPlus :: [Ast] -> Maybe Ast
evalPlus = evalMathOperator (+)

evalMinus :: [Ast] -> Maybe Ast
evalMinus = evalMathOperator (-)

evalMul :: [Ast] -> Maybe Ast
evalMul = evalMathOperator (*)

evalDiv :: [Ast] -> Maybe Ast
evalDiv = evalMathOperator (/)

evalMod :: [Ast] -> Maybe Ast
evalMod [Value (Number x), Value (Number y)] = Just (Value (Number (x `mod` y)))
evalMod _ = Nothing

evalEq :: [Ast] -> Maybe Ast
evalEq [Value (Float x), Value (Float y)] = Just (Value (Bool ((==) x y)))
evalEq [Value (Float x), Value (Number y)] = Just (Value (Bool ((==) x (int2Double y))))
evalEq [Value (Number x), Value (Number y)] = Just (Value (Bool ((==) x y)))
evalEq [Value (Bool x), Value (Bool y)] = Just (Value (Bool ((==) x y)))
evalEq [Value (Bool x), Value (Number y)] = Just (Value (Bool ((==) x (y /= 0))))
evalEq [Value (Bool x), Value (Float y)] = Just (Value (Bool ((==) x (y /= 0))))
evalEq [Value (Number x), Value (Bool y)] = Just (Value (Bool ((==) (x /= 0) y)))
evalEq [Value (Float x), Value (Bool y)] = Just (Value (Bool ((==) (x /= 0) y)))
evalEq [Value (Number x), Value (Float y)] = Just (Value (Bool ((==) (int2Double x) y)))
evalEq _ = Nothing

evalInf :: [Ast] -> Maybe Ast
evalInf [Value (Float x), Value (Float y)] = Just (Value (Bool ((<) x y)))
evalInf [Value (Float x), Value (Number y)] = Just (Value (Bool ((<) x (int2Double y))))
evalInf [Value (Number x), Value (Number y)] = Just (Value (Bool ((<) x y)))
evalInf [Value (Bool x), Value (Bool y)] = Just (Value (Bool ((<) x y)))
evalInf [Value (Number x), Value (Float y)] = Just (Value (Bool ((<) (int2Double x) y)))
evalInf _ = Nothing

maths :: [(String, [Ast] -> Maybe Ast)]
maths =
    [ ("+", evalPlus),
      ("-", evalMinus),
      ("*", evalMul),
      ("div", evalDiv),
      ("eq?", evalEq),
      ("<", evalInf),
      ("mod", evalMod)
    ]

findFuncAndExec' :: [(String, [Ast] -> Maybe Ast)] -> String -> [Ast] -> Maybe Ast
findFuncAndExec' ((s, f) : _) n ag | s == n = f ag
findFuncAndExec' (_ : xs) n ag = findFuncAndExec' xs n ag
findFuncAndExec' [] _ _ = Nothing

findFuncAndExec :: Function -> Maybe Ast
findFuncAndExec (Function s a) = mapM evalAST a >>= findFuncAndExec' maths s

sexprToAST :: Sexpr Int Double -> Maybe Ast
sexprToAST (Atom i) = Just (Value i)
sexprToAST (List ((Atom (String s)) : xs)) =
    mapM sexprToAST xs >>= (\x -> Just (Call $ Function s x))
sexprToAST _ = Nothing

evalAST :: Ast -> Maybe Ast
evalAST (Value x) = Just (Value x)
evalAST (Call f@(Function _ _)) = findFuncAndExec f
evalAST _ = Nothing
