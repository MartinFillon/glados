{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Maths
-}

module Eval.Ops (evalAdd, evalAnd, evalBAnd, evalBOr, evalBXor, evalGreaterThan, evalSub, evalMul, evalOr, evalPower, evalDiv, evalEq, evalLessThan, evalMod, evalNEq, evalShiftL, evalShiftR, isNumeric) where

import Data.Bits (Bits (shiftL, shiftR, xor, (.|.)), (.&.))
import Data.Data (typeOf)
import Data.Fixed (mod')
import Debug.Trace (trace)
import GHC.Float (double2Int, int2Double)
import Memory (Memory)
import Parsing.ParserAst (Ast (..))

isNumeric :: Ast -> Bool
isNumeric (AstInt _) = True
isNumeric (AstDouble _) = True
isNumeric _ = False

-- Logical

evalEq :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalEq mem left right = Right (AstBool (left == right), mem)
evalEq _ _ _ = Left "Invalid arguments for `==`."

evalNEq :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalNEq mem left right = Right (AstBool (left /= right), mem)
evalNEq _ _ _ = Left "Invalid arguments for `!=`."

evalLessThan :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalLessThan mem (AstInt i1) (AstInt i2) = Right (AstBool (i1 < i2), mem)
evalLessThan mem (AstDouble d1) (AstDouble d2) = Right (AstBool (d1 < d2), mem)
evalLessThan mem (AstInt i) (AstDouble d) = Right (AstBool (int2Double i < d), mem)
evalLessThan mem (AstDouble d) (AstInt i) = Right (AstBool (d < int2Double i), mem)
evalLessThan _ a b
    | isNumeric a =
        Left ("Argument \"" ++ show b ++ "\" invalid for `<`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `<`.")
evalLessThan _ _ _ = Left "Invalid arguments for `<`."

evalGreaterThan :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalGreaterThan mem (AstInt i1) (AstInt i2) = Right (AstBool (i1 > i2), mem)
evalGreaterThan mem (AstDouble d1) (AstDouble d2) = Right (AstBool (d1 > d2), mem)
evalGreaterThan mem (AstInt i) (AstDouble d) = Right (AstBool (int2Double i > d), mem)
evalGreaterThan mem (AstDouble d) (AstInt i) = Right (AstBool (d > int2Double i), mem)
evalGreaterThan _ a b
    | isNumeric a =
        Left ("Argument \"" ++ show b ++ "\" invalid for `>`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `>`.")
evalGreaterThan _ _ _ = Left "Invalid arguments for `>`."

-- Binary

evalBAnd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalBAnd mem (AstInt i1) (AstInt i2) =
    Right (AstInt $ (.&.) (fromIntegral i1) (fromIntegral i2), mem)
evalBAnd _ a b
    | typeOf a == typeOf AstInt =
        Left ("Argument \"" ++ show b ++ "\" invalid for binary `and`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for binary `and`.")
evalBAnd _ _ _ = Left "Invalid arguments for binary `and`."

evalBOr :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalBOr mem (AstInt i1) (AstInt i2) =
    Right (AstInt $ (.|.) (fromIntegral i1) (fromIntegral i2), mem)
evalBOr _ a b
    | typeOf a == typeOf AstInt =
        Left ("Argument \"" ++ show b ++ "\" invalid for binary `or`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for binary `or`.")
evalBOr _ _ _ = Left "Invalid arguments for binary `or`."

evalBXor :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalBXor mem (AstInt i1) (AstInt i2) =
    Right (AstInt $ xor (fromIntegral i1) (fromIntegral i2), mem)
evalBXor _ a b
    | typeOf a == typeOf AstInt =
        Left ("Argument \"" ++ show b ++ "\" invalid for `xor`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `xor`.")
evalBXor _ _ _ = Left "Invalid arguments for `xor`."

evalShiftR :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalShiftR mem (AstInt i1) (AstInt i2) =
    Right (AstInt $ shiftR (fromIntegral i1) (fromIntegral i2), mem)
evalShiftR _ a b
    | typeOf a == typeOf AstInt =
        Left ("Argument \"" ++ show b ++ "\" invalid for `>>`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `>>`.")
evalShiftR _ _ _ = Left "Invalid arguments for `>>`."

evalShiftL :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalShiftL mem (AstInt i1) (AstInt i2) = Right (AstInt $ shiftL (fromIntegral i1) (fromIntegral i2), mem)
evalShiftL _ a b
    | typeOf a == typeOf AstInt =
        Left ("Argument \"" ++ show b ++ "\" invalid for `<<`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `<<`.")
evalShiftL _ _ _ = Left "Invalid arguments for `<<`."

-- Booleans

evalAnd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAnd mem (AstBool b1) (AstBool b2) = Right (AstBool (b1 && b2), mem)
evalAnd _ a b
    | typeOf a == typeOf AstBool =
        Left ("Argument \"" ++ show b ++ "\" invalid for `and`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `and`.")
evalAnd _ _ _ = Left "Invalid arguments for `and`."

evalOr :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalOr mem (AstBool b1) (AstBool b2) = Right (AstBool (b1 || b2), mem)
evalOr _ a b
    | typeOf a == typeOf AstBool =
        Left ("Argument \"" ++ show b ++ "\" invalid for `or`.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for `or`.")
evalOr _ _ _ = Left "Invalid arguments for `or`."

-- Maths

evalMath ::
    String ->
    (Double -> Double -> Double) ->
    Memory ->
    Ast ->
    Ast ->
    Either String (Ast, Memory)
evalMath _ f mem (AstInt i1) (AstInt i2) =
    Right (AstInt (double2Int (f (int2Double i1) (int2Double i2))), mem)
evalMath _ f mem (AstDouble d1) (AstDouble d2) =
    Right (AstDouble (f d1 d2), mem)
evalMath _ f mem (AstInt i) (AstDouble d) =
    Right (AstDouble (f (int2Double i) d), mem)
evalMath _ f mem (AstDouble d) (AstInt i) =
    Right (AstDouble (f d (int2Double i)), mem)
evalMath opname _ _ a b
    | isNumeric a =
        Left ("Argument \"" ++ show b ++ "\" invalid for operation " ++ opname ++ ".")
    | otherwise =
        Left ("Argument \"" ++ show a ++ "\" invalid for operation " ++ opname ++ ".")
evalMath opname _ _ _ _ = Left ("Invalid arguments for " ++ opname ++ ".")

evalAdd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAdd = evalMath "+" (+)

evalSub :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalSub = evalMath "-" (-)

evalMul :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalMul = evalMath "*" (*)

evalPower :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalPower = evalMath "**" (**)

evalDiv :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalDiv mem (AstInt i1) (AstInt i2)
    | i2 /= 0 = Right (AstInt (i1 `quot` i2), mem)
    | otherwise = Left "Division by zero."
evalDiv mem (AstDouble d1) (AstDouble d2)
    | d2 /= 0 = Right (AstDouble (d1 / d2), mem)
    | otherwise = Left "Division by zero."
evalDiv mem (AstInt i) (AstDouble d)
    | d /= 0 = Right (AstDouble (int2Double i / d), mem)
    | otherwise = Left "Division by zero."
evalDiv mem (AstDouble d) (AstInt i)
    | i /= 0 = Right (AstDouble (d / int2Double i), mem)
    | otherwise = Left "Division by zero."
evalDiv _ a b
    | isNumeric a =
        Left ("Argument \"" ++ show b ++ "\" invalid for division.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for division.")
evalDiv _ _ _ = Left "Invalid arguments for division."

evalMod :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalMod mem (AstInt i1) (AstInt i2)
    | i2 /= 0 = Right (AstInt (i1 `mod` i2), mem)
    | otherwise = Left "Modulo by zero."
evalMod mem (AstDouble d1) (AstDouble d2)
    | d2 /= 0 = Right (AstDouble (d1 `mod'` d2), mem)
    | otherwise = Left "Modulo by zero."
evalMod mem (AstInt i) (AstDouble d)
    | d /= 0 = Right (AstDouble (int2Double i `mod'` d), mem)
    | otherwise = Left "Modulo by zero."
evalMod mem (AstDouble d) (AstInt i)
    | i /= 0 = Right (AstDouble (d `mod'` int2Double i), mem)
    | otherwise = Left "Modulo by zero."
evalMod _ a b
    | isNumeric a =
        Left ("Argument \"" ++ show b ++ "\" invalid for modulo.")
    | otherwise = Left ("Argument \"" ++ show a ++ "\" invalid for modulo.")
evalMod _ _ _ = Left "Invalid arguments for modulo."
