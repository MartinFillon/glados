{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Maths
-}

module Eval.Maths (evalAdd, evalSub, evalMul, evalDiv, evalMod) where

import Data.Fixed (mod')
import GHC.Float (double2Int, int2Double)
import Memory (Memory)
import Parsing.SExprToAst (Ast (..))

evalMath :: String -> (Double -> Double -> Double) -> Memory -> [Ast] -> Either String (Ast, Memory)
evalMath _ f mem [AstInt i1, AstInt i2] =
    Right (AstInt (double2Int (f (int2Double i1) (int2Double i2))), mem)
evalMath _ f mem [AstFloat f1, AstFloat f2] =
    Right (AstFloat (f f1 f2), mem)
evalMath _ f mem [AstInt i, AstFloat f1] =
    Right (AstFloat (f (int2Double i) f1), mem)
evalMath _ f mem [AstFloat f1, AstInt i] =
    Right (AstFloat (f f1 (int2Double i)), mem)
evalMath opname _ _ _ = Left ("Invalid arguments for operation " ++ opname)

evalAdd :: Memory -> [Ast] -> Either String (Ast, Memory)
evalAdd = evalMath "+" (+)

evalSub :: Memory -> [Ast] -> Either String (Ast, Memory)
evalSub = evalMath "-" (-)

evalMul :: Memory -> [Ast] -> Either String (Ast, Memory)
evalMul = evalMath "*" (*)

evalDiv :: Memory -> [Ast] -> Either String (Ast, Memory)
evalDiv mem [AstInt i1, AstInt i2]
    | i2 /= 0 = Right (AstInt (i1 `quot` i2), mem)
    | otherwise = Left "Division by zero"
evalDiv mem [AstFloat f1, AstFloat f2]
    | f2 /= 0 = Right (AstFloat (f1 / f2), mem)
    | otherwise = Left "Division by zero"
evalDiv mem [AstInt i, AstFloat f]
    | f /= 0 = Right (AstFloat (fromIntegral i / f), mem)
    | otherwise = Left "Division by zero"
evalDiv mem [AstFloat f, AstInt i]
    | i /= 0 = Right (AstFloat (f / fromIntegral i), mem)
    | otherwise = Left "Division by zero"
evalDiv _ _ = Left "Invalid arguments for division"

evalMod :: Memory -> [Ast] -> Either String (Ast, Memory)
evalMod mem [AstInt i1, AstInt i2]
    | i2 /= 0 = Right (AstInt (i1 `mod` i2), mem)
    | otherwise = Left "Modulo by zero"
evalMod mem [AstFloat f1, AstFloat f2]
    | f2 /= 0 = Right (AstFloat (f1 `mod'` f2), mem)
    | otherwise = Left "Modulo by zero"
evalMod mem [AstInt i, AstFloat f]
    | f /= 0 = Right (AstFloat (fromIntegral i `mod'` f), mem)
    | otherwise = Left "Modulo by zero"
evalMod mem [AstFloat f, AstInt i]
    | i /= 0 = Right (AstFloat (f `mod'` fromIntegral i), mem)
    | otherwise = Left "Modulo by zero"
evalMod _ _ = Left "Invalid arguments for modulo"
