{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Maths
-}

module Eval.Ops (evalAdd, evalSub, evalMul, evalDiv, evalMod, isNumeric) where

import Data.Fixed (mod')
import Debug.Trace (trace)
import GHC.Float (double2Int, int2Double)
import Memory (Memory)
import Parsing.ParserAst (Ast (..))

-- Booleans

--TODO

-- Maths

isNumeric :: Ast -> Bool
isNumeric (AstInt _) = True
isNumeric (AstDouble _) = True
isNumeric _ = False

evalMath ::
    String ->
    (Double -> Double -> Double) ->
    Memory ->
    Ast ->
    Ast ->
    Either String (Ast, Memory)
evalMath _ f mem (AstInt i1) (AstInt i2) =
    Right (AstInt (double2Int (f (int2Double i1) (int2Double i2))), mem)
evalMath _ f mem (AstDouble f1) (AstDouble f2) =
    Right (AstDouble (f f1 f2), mem)
evalMath _ f mem (AstInt i) (AstDouble f1) =
    Right (AstDouble (f (int2Double i) f1), mem)
evalMath _ f mem (AstDouble f1) (AstInt i) =
    Right (AstDouble (f f1 (int2Double i)), mem)
evalMath opname _ _ a b
    | isNumeric a =
        Left ("Arguments " ++ show b ++ " out of bound for operation " ++ opname)
    | otherwise =
        Left ("Arguments " ++ show a ++ " out of bound for operation " ++ opname)

evalAdd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAdd = evalMath "+" (+)

evalSub :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalSub = evalMath "-" (-)

evalMul :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalMul = evalMath "*" (*)

evalDiv :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalDiv mem (AstInt i1) (AstInt i2)
    | i2 /= 0 = Right (AstInt (i1 `quot` i2), mem)
    | otherwise = Left "Division by zero"
evalDiv mem (AstDouble f1) (AstDouble f2)
    | f2 /= 0 = Right (AstDouble (f1 / f2), mem)
    | otherwise = Left "Division by zero"
evalDiv mem (AstInt i) (AstDouble f)
    | f /= 0 = Right (AstDouble (int2Double i / f), mem)
    | otherwise = Left "Division by zero"
evalDiv mem (AstDouble f) (AstInt i)
    | i /= 0 = Right (AstDouble (f / int2Double i), mem)
    | otherwise = Left "Division by zero"
evalDiv _ a b
    | isNumeric a =
        Left ("Arguments " ++ show b ++ " out of bound for division")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for division")

evalMod :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalMod mem (AstInt i1) (AstInt i2)
    | i2 /= 0 = Right (AstInt (i1 `mod` i2), mem)
    | otherwise = Left "Modulo by zero"
evalMod mem (AstDouble f1) (AstDouble f2)
    | f2 /= 0 = Right (AstDouble (f1 `mod'` f2), mem)
    | otherwise = Left "Modulo by zero"
evalMod mem (AstInt i) (AstDouble f)
    | f /= 0 = Right (AstDouble (int2Double i `mod'` f), mem)
    | otherwise = Left "Modulo by zero"
evalMod mem (AstDouble f) (AstInt i)
    | i /= 0 = Right (AstDouble (f `mod'` int2Double i), mem)
    | otherwise = Left "Modulo by zero"
evalMod _ a b
    | isNumeric a =
        Left ("Arguments " ++ show b ++ " out of bound for modulo")
    | otherwise = Left ("Arguments " ++ show a ++ " out of bound for modulo")
