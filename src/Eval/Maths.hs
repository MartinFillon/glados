{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Maths
-}

module Eval.Maths (evalAdd, evalSub, evalMul, evalDiv, evalMod) where

import Data.Fixed (mod')
import GHC.Float (double2Int, int2Double)
import Parsing.SExprToAst (Ast (..))

evalMath :: (Double -> Double -> Double) -> [Ast] -> Maybe Ast
evalMath f [AstInt i1, AstInt i2] = Just (AstInt (double2Int (f (int2Double i1) (int2Double i2))))
evalMath f [AstFloat f1, AstFloat f2] = Just (AstFloat (f f1 f2))
evalMath f [AstInt i, AstFloat f1] = Just (AstFloat (f (int2Double i) f1))
evalMath f [AstFloat f1, AstInt i] = Just (AstFloat (f f1 (int2Double i)))
evalMath _ _ = Nothing

evalAdd :: [Ast] -> Maybe Ast
evalAdd = evalMath (+)

evalSub :: [Ast] -> Maybe Ast
evalSub = evalMath (-)

evalMul :: [Ast] -> Maybe Ast
evalMul = evalMath (*)

evalDiv :: [Ast] -> Maybe Ast
evalDiv [AstInt i1, AstInt i2]
    | i2 /= 0 = Just (AstInt (i1 `div` i2))
    | otherwise = Nothing
evalDiv [AstFloat f1, AstFloat f2]
    | f2 /= 0 = Just (AstFloat (f1 / f2))
    | otherwise = Nothing
evalDiv [AstInt i, AstFloat f]
    | f /= 0 = Just (AstFloat (fromIntegral i / f))
    | otherwise = Nothing
evalDiv [AstFloat f, AstInt i]
    | i /= 0 = Just (AstFloat (f / fromIntegral i))
    | otherwise = Nothing
evalDiv _ = Nothing

evalMod :: [Ast] -> Maybe Ast
evalMod [AstInt i1, AstInt i2]
    | i2 /= 0 = Just (AstInt (i1 `mod` i2))
    | otherwise = Nothing
evalMod [AstFloat f1, AstFloat f2]
    | f2 /= 0 = Just (AstFloat (f1 `mod'` f2))
    | otherwise = Nothing
evalMod [AstInt i, AstFloat f]
    | f /= 0 = Just (AstFloat (fromIntegral i `mod'` f))
    | otherwise = Nothing
evalMod [AstFloat f, AstInt i]
    | i /= 0 = Just (AstFloat (f `mod'` fromIntegral i))
    | otherwise = Nothing
evalMod _ = Nothing
