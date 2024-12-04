module Eval.Maths (evalAdd, evalSub, evalMul, evalDiv, evalMod) where

import Data.Fixed (mod')
import GHC.Float (double2Int, int2Double)
import Parsing.SExprToAst (Ast (..))

evalMath :: String -> (Double -> Double -> Double) -> [Ast] -> Either String Ast
evalMath _ f [AstInt i1, AstInt i2] =
    Right (AstInt (double2Int (f (int2Double i1) (int2Double i2))))
evalMath _ f [AstFloat f1, AstFloat f2] =
    Right (AstFloat (f f1 f2))
evalMath _ f [AstInt i, AstFloat f1] =
    Right (AstFloat (f (int2Double i) f1))
evalMath _ f [AstFloat f1, AstInt i] =
    Right (AstFloat (f f1 (int2Double i)))
evalMath opname _ _ = Left ("Invalid arguments for operation " ++ opname)

evalAdd :: [Ast] -> Either String Ast
evalAdd = evalMath "+" (+)

evalSub :: [Ast] -> Either String Ast
evalSub = evalMath "-" (-)

evalMul :: [Ast] -> Either String Ast
evalMul = evalMath "*" (*)

evalDiv :: [Ast] -> Either String Ast
evalDiv [AstInt i1, AstInt i2]
    | i2 /= 0 = Right (AstInt (i1 `quot` i2))
    | otherwise = Left "Division by zero"
evalDiv [AstFloat f1, AstFloat f2]
    | f2 /= 0 = Right (AstFloat (f1 / f2))
    | otherwise = Left "Division by zero"
evalDiv [AstInt i, AstFloat f]
    | f /= 0 = Right (AstFloat (fromIntegral i / f))
    | otherwise = Left "Division by zero"
evalDiv [AstFloat f, AstInt i]
    | i /= 0 = Right (AstFloat (f / fromIntegral i))
    | otherwise = Left "Division by zero"
evalDiv _ = Left "Invalid arguments for division"

evalMod :: [Ast] -> Either String Ast
evalMod [AstInt i1, AstInt i2]
    | i2 /= 0 = Right (AstInt (i1 `mod` i2))
    | otherwise = Left "Modulo by zero"
evalMod [AstFloat f1, AstFloat f2]
    | f2 /= 0 = Right (AstFloat (f1 `mod'` f2))
    | otherwise = Left "Modulo by zero"
evalMod [AstInt i, AstFloat f]
    | f /= 0 = Right (AstFloat (fromIntegral i `mod'` f))
    | otherwise = Left "Modulo by zero"
evalMod [AstFloat f, AstInt i]
    | i /= 0 = Right (AstFloat (f `mod'` fromIntegral i))
    | otherwise = Left "Modulo by zero"
evalMod _ = Left "Invalid arguments for modulo"
