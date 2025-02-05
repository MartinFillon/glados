{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Maths
-}

module Eval.Ops (
    applyOp,
    assocOpExpectation,
    boolTokens,
    evalOpExpr,
    evalBinaryRet,
    isNumeric,
) where

import Data.Bits (Bits (shiftL, shiftR, xor, (.|.)), (.&.))
import Data.Data (typeOf)
import Data.Fixed (mod')
import qualified Data.Map as Map
import GHC.Float (double2Int, int2Double)
import Memory (Memory)
import Parsing.ParserAst (
    Ast (..),
    Function (..),
    MarylType (..),
    Variable (..),
 )

type FunctionRegistry =
    Map.Map String (Memory -> Ast -> Ast -> Either String (Ast, Memory))

boolTokens :: [Char]
boolTokens = ['=', '!', '<', '>']

-- | Associate operation to its valid return types
assocOpExpectation :: Map.Map String [MarylType]
assocOpExpectation =
    Map.fromList
        [ ("+", [Int, Double]),
          ("-", [Int, Double]),
          ("*", [Int, Double]),
          ("/", [Int, Double]),
          ("%", [Int, Double]),
          ("**", [Int, Double]),
          ("<", [Bool]),
          ("<=", [Bool]),
          (">", [Bool]),
          (">=", [Bool]),
          ("==", [Bool]),
          ("!=", [Bool]),
          ("and", [Bool]),
          ("or", [Bool]),
          ("&", [Int]),
          ("|", [Int]),
          ("^", [Int]),
          ("<<", [Int]),
          (">>", [Int])
        ]

-- | Evaluate the possible expected types from its operation.
evalOpExpr :: String -> Either String [MarylType]
evalOpExpr op = case Map.lookup op assocOpExpectation of
    Just expectedTypes -> Right expectedTypes
    _ -> Left "Operation couldn't be found."

-- | Evaluate the return value of a binary function of two inputs
evalBinaryRet :: String -> MarylType -> Memory -> Either String ()
evalBinaryRet op expectedType _ =
    case Map.lookup op assocOpExpectation of
        Just expectedTypes ->
            if expectedType `elem` expectedTypes
                then Right ()
                else
                    Left
                        ( "Operation \""
                            ++ op
                            ++ "\" does not justify expected type "
                            ++ show expectedType
                            ++ "."
                        )
        Nothing -> Left ("Unknown operator \"" ++ op ++ "\".")

defaultRegistry :: FunctionRegistry
defaultRegistry =
    Map.fromList
        [ ("+", evalAdd),
          ("-", evalSub),
          ("*", evalMul),
          ("/", evalDiv),
          ("%", evalMod),
          ("**", evalPower),
          ("|", evalBOr),
          ("&", evalBAnd),
          (">>", evalShiftR),
          ("<<", evalShiftL),
          ("^", evalBXor),
          ("==", evalEq),
          ("!=", evalNEq),
          (">", evalGreaterThan),
          (">=", evalGreatThanEq),
          ("<", evalLessThan),
          ("<=", evalLessThanEq),
          ("or", evalOr),
          ("and", evalAnd)
        ]

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

-- | Apply the operation string to the associated function.
applyOp :: Memory -> String -> Ast -> Ast -> Either String (Ast, Memory)
applyOp mem op left right =
    maybeToEither
        (op ++ " is not a valid operator")
        (Map.lookup op defaultRegistry)
        >>= (\f -> f mem left right)

-- Logical

-- | ==
evalEq :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalEq mem left right = Right (AstBool (left == right), mem)

-- | !=
evalNEq :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalNEq mem left right = Right (AstBool (left /= right), mem)

-- | <
evalLessThan :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalLessThan mem (AstInt i1) (AstInt i2) = Right (AstBool (i1 < i2), mem)
evalLessThan mem (AstDouble d1) (AstDouble d2) = Right (AstBool (d1 < d2), mem)
evalLessThan mem (AstInt i) (AstDouble d) = Right (AstBool (int2Double i < d), mem)
evalLessThan mem (AstDouble d) (AstInt i) = Right (AstBool (d < int2Double i), mem)
evalLessThan mem l r
    | isNumeric l && isNumeric r =
        Right (AstBinaryFunc "<" l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for \"<\"."
            )

-- | >
evalGreaterThan :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalGreaterThan mem (AstInt i1) (AstInt i2) = Right (AstBool (i1 > i2), mem)
evalGreaterThan mem (AstDouble d1) (AstDouble d2) = Right (AstBool (d1 > d2), mem)
evalGreaterThan mem (AstInt i) (AstDouble d) = Right (AstBool (int2Double i > d), mem)
evalGreaterThan mem (AstDouble d) (AstInt i) = Right (AstBool (d > int2Double i), mem)
evalGreaterThan mem l r
    | isNumeric l && isNumeric r =
        Right (AstBinaryFunc ">" l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for \">\"."
            )

-- | >=
evalGreatThanEq :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalGreatThanEq mem (AstInt i1) (AstInt i2) = Right (AstBool (i1 >= i2), mem)
evalGreatThanEq mem (AstDouble d1) (AstDouble d2) = Right (AstBool (d1 >= d2), mem)
evalGreatThanEq mem (AstInt i) (AstDouble d) = Right (AstBool (int2Double i >= d), mem)
evalGreatThanEq mem (AstDouble d) (AstInt i) = Right (AstBool (d >= int2Double i), mem)
evalGreatThanEq mem l r
    | isNumeric l && isNumeric r =
        Right (AstBinaryFunc ">=" l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for \">=\"."
            )

-- | <=
evalLessThanEq :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalLessThanEq mem (AstInt i1) (AstInt i2) = Right (AstBool (i1 <= i2), mem)
evalLessThanEq mem (AstDouble d1) (AstDouble d2) = Right (AstBool (d1 <= d2), mem)
evalLessThanEq mem (AstInt i) (AstDouble d) = Right (AstBool (int2Double i <= d), mem)
evalLessThanEq mem (AstDouble d) (AstInt i) = Right (AstBool (d <= int2Double i), mem)
evalLessThanEq mem l r
    | isNumeric l && isNumeric r =
        Right (AstBinaryFunc "<=" l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for \"<=\"."
            )

-- Binary

evalBinaryOp ::
    String ->
    (Int -> Int -> Int) ->
    Memory ->
    Ast ->
    Ast ->
    Either String (Ast, Memory)
evalBinaryOp _ op mem (AstInt i1) (AstInt i2) = Right (AstInt (op i1 i2), mem)
evalBinaryOp opname _ mem (AstArg var idx) (AstInt i)
    | isInt var = Right (AstBinaryFunc opname (AstArg var idx) (AstInt i), mem)
    | otherwise =
        Left (show var ++ " isn't of valid type for " ++ opname ++ ", expected Int.")
evalBinaryOp opname _ mem (AstInt i) (AstArg var idx)
    | isInt var = Right (AstBinaryFunc opname (AstInt i) (AstArg var idx), mem)
    | otherwise =
        Left (show var ++ " isn't of valid type for " ++ opname ++ ", expected Int.")
evalBinaryOp opname _ mem (AstArg v1 i1) (AstArg v2 i2)
    | isInt v1 && isInt v2 =
        Right (AstBinaryFunc opname (AstArg v1 i1) (AstArg v2 i2), mem)
    | otherwise =
        Left
            ( show v1
                ++ " and/or "
                ++ show v2
                ++ " aren't of valid type for "
                ++ opname
                ++ ", expected Int."
            )
evalBinaryOp op _ _ a b
    | typeOf a == typeOf AstInt =
        Left ("Argument \"" ++ show b ++ "\" invalid for binary " ++ show op ++ ".")
    | otherwise =
        Left ("Argument \"" ++ show a ++ "\" invalid for binary " ++ show op ++ ".")

-- | &
evalBAnd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalBAnd = evalBinaryOp "&" (.&.)

-- | |
evalBOr :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalBOr = evalBinaryOp "|" (.|.)

-- | ^
evalBXor :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalBXor = evalBinaryOp "^" xor

-- | <<
evalShiftL :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalShiftL = evalBinaryOp "<<" shiftL

-- | >>
evalShiftR :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalShiftR = evalBinaryOp ">>" shiftR

-- Booleans

evalBooleanOp ::
    String ->
    (Bool -> Bool -> Bool) ->
    Memory ->
    Ast ->
    Ast ->
    Either String (Ast, Memory)
evalBooleanOp _ op mem (AstBool b1) (AstBool b2) = Right (AstBool (op b1 b2), mem)
evalBooleanOp opname _ mem (AstArg var idx) (AstBool b)
    | isBool var = Right (AstBinaryFunc opname (AstArg var idx) (AstBool b), mem)
    | otherwise =
        Left (show var ++ " isn't of valid type for " ++ opname ++ ", expected Bool.")
evalBooleanOp opname _ mem (AstBool b) (AstArg var idx)
    | isBool var = Right (AstBinaryFunc opname (AstBool b) (AstArg var idx), mem)
    | otherwise =
        Left (show var ++ " isn't of valid type for " ++ opname ++ ", expected Bool.")
evalBooleanOp opname _ mem (AstArg v1 i1) (AstArg v2 i2)
    | isBool v1 && isBool v2 =
        Right (AstBinaryFunc opname (AstArg v1 i1) (AstArg v2 i2), mem)
    | otherwise =
        Left
            ( show v1
                ++ " and/or "
                ++ show v2
                ++ " aren't of valid type for "
                ++ opname
                ++ ", expected Bool."
            )
evalBooleanOp opname _ _ a b
    | typeOf a == typeOf AstBool =
        Left ("Argument \"" ++ show b ++ "\" invalid for " ++ show opname ++ ".")
    | otherwise =
        Left ("Argument \"" ++ show a ++ "\" invalid for " ++ show opname ++ ".")

-- | and (&&)
evalAnd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAnd = evalBooleanOp "and" (&&)

-- | or (||)
evalOr :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalOr = evalBooleanOp "or" (||)

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
evalMath _ f mem (AstDouble d1) (AstDouble d2) = Right (AstDouble (f d1 d2), mem)
evalMath _ f mem (AstInt i) (AstDouble d) = Right (AstDouble (f (int2Double i) d), mem)
evalMath _ f mem (AstDouble d) (AstInt i) = Right (AstDouble (f d (int2Double i)), mem)
evalMath opname _ mem l r
    | isNumeric l && isNumeric r =
        Right (AstBinaryFunc opname l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for operation "
                ++ opname
                ++ "."
            )

evalAdd :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalAdd = evalMath "+" (+)

evalSub :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalSub = evalMath "-" (-)

evalMul :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalMul = evalMath "*" (*)

evalPower :: Memory -> Ast -> Ast -> Either String (Ast, Memory)
evalPower = evalMath "**" (**)

-- | /
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
evalDiv mem l r
    | isNumeric l && isNumeric r =
        Right (AstBinaryFunc "/" l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for division."
            )

-- | %
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
evalMod mem l r
    | isNumeric l && isNumeric r = Right (AstBinaryFunc "%" l r, mem)
    | otherwise =
        Left
            ( "Arguments "
                ++ show l
                ++ " or/and "
                ++ show r
                ++ " is of invalid type for modulo."
            )

isNumeric :: Ast -> Bool
isNumeric (AstInt _) = True
isNumeric (AstDouble _) = True
isNumeric (AstArg (AstDefineVar (Variable _ Int _)) _) = True
isNumeric (AstArg (AstDefineVar (Variable _ Double _)) _) = True
isNumeric (AstArg (AstFunc (Function _ _ _ Int)) _) = True
isNumeric (AstArg (AstFunc (Function _ _ _ Double)) _) = True
isNumeric (AstArg (AstInt _) _) = True
isNumeric (AstArg (AstDouble _) _) = True
isNumeric (AstArg (AstDefineVar (Variable _ (Const Int) _)) _) = True
isNumeric (AstArg (AstDefineVar (Variable _ (Const Double) _)) _) = True
isNumeric (AstDefineVar (Variable _ Int _)) = True
isNumeric (AstDefineVar (Variable _ Double _)) = True
isNumeric (AstDefineVar (Variable _ (Const Int) _)) = True
isNumeric (AstDefineVar (Variable _ (Const Double) _)) = True
isNumeric (AstFunc (Function _ _ _ Int)) = True
isNumeric (AstFunc (Function _ _ _ Double)) = True
isNumeric (AstConst (AstDefineVar (Variable _ Int _))) = True
isNumeric (AstConst (AstDefineVar (Variable _ Double _))) = True
isNumeric (AstGlobal (AstDefineVar (Variable _ Int _))) = True
isNumeric (AstGlobal (AstDefineVar (Variable _ Double _))) = True
isNumeric (AstGlobal (AstInt _)) = True
isNumeric (AstGlobal (AstDouble _)) = True
isNumeric _ = False

isInt :: Ast -> Bool
isInt (AstInt _) = True
isInt (AstArg (AstDefineVar (Variable _ Int _)) _) = True
isInt (AstArg (AstFunc (Function _ _ _ Int)) _) = True
isInt (AstArg (AstInt _) _) = True
isInt (AstArg (AstDefineVar (Variable _ (Const Int) _)) _) = True
isInt (AstArg (AstDefineVar (Variable _ (Const Double) _)) _) = True
isInt (AstDefineVar (Variable _ Int _)) = True
isInt (AstDefineVar (Variable _ (Const Int) _)) = True
isInt (AstFunc (Function _ _ _ Int)) = True
isInt (AstConst (AstDefineVar (Variable _ Int _))) = True
isInt (AstGlobal (AstDefineVar (Variable _ Int _))) = True
isInt (AstGlobal (AstInt _)) = True
isInt _ = False

isBool :: Ast -> Bool
isBool (AstBool _) = True
isBool (AstArg (AstDefineVar (Variable _ Bool _)) _) = True
isBool (AstArg (AstFunc (Function _ _ _ Bool)) _) = True
isBool (AstArg (AstBool _) _) = True
isBool (AstArg (AstDefineVar (Variable _ (Const Bool) _)) _) = True
isBool (AstDefineVar (Variable _ Bool _)) = True
isBool (AstFunc (Function _ _ _ Bool)) = True
isBool (AstConst (AstDefineVar (Variable _ Bool _))) = True
isBool (AstGlobal (AstDefineVar (Variable _ Bool _))) = True
isBool (AstGlobal (AstBool _)) = True
isBool _ = False
