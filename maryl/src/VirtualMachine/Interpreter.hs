{-
-- EPITECH PROJECT, 2024
-- inst [WSL: Ubuntu]
-- File description:
-- interpreter
-}

module VirtualMachine.Interpreter (
    Numeric(..),
    Value(..),
    Inst(..),
    Stack,
    Insts,
    Args,
    Memory,
    initialMemory,
    exec
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int (Int64)

class Numeric a where
    toDouble :: a -> Double
    fromDouble :: Double -> Either String a

instance Numeric Int64 where
    toDouble = fromIntegral
    fromDouble d = if (fromIntegral (round d :: Int64) :: Double) == d
        then Right (round d :: Int64)
        else Left "Cant convert to Int64 without loss"


instance Numeric Double where
    toDouble = id
    fromDouble = Right

data Value
    = N Int64
    | B Bool
    | S String
    | L [Value]
    | D Double
    | F Float
    | Op ([Value] -> Either String Value)
    | Bi Insts

instance Show Value where
    show (N n) = show n
    show (B b) = show b
    show (S s) = show s
    show (L vs) = show vs
    show (D d) = show d
    show (F f) = show f
    show (Op _) = "<operator>"
    show (Bi _) = "<builtin>"

instance Eq Value where
    (N a) == (N b) = a == b
    (B a) == (B b) = a == b
    (S a) == (S b) = a == b
    (L a) == (L b) = a == b
    (D a) == (D b) = a == b
    (F a) == (F b) = a == b
    (Bi a) == (Bi b) = a == b
    _ == _ = False

data Inst
    = Noop
    | Push Value
    | PushArg Int
    | Call String
    | Ret
    | JumpIfFalse Int
    deriving (Show, Eq)

type Stack = [Value]
type Insts = [Inst]
type Args = [Value]
type Memory = Map String Value

    -------------------------
    -- operator operations --
    -------------------------

instance Numeric Float where
    toDouble = realToFrac
    fromDouble = Right . realToFrac

numericOp :: (Double -> Double -> Double) -> Value -> Value -> Either String Value
numericOp op (N x) (N y) = case fromDouble (op (toDouble x) (toDouble y)) of
    Right n -> Right $ N n
    Left _ -> Right $ D (op (toDouble x) (toDouble y))
numericOp op (N x) (D y) = Right $ D (op (toDouble x) y)
numericOp op (D x) (N y) = Right $ D (op x (toDouble y))
numericOp op (D x) (D y) = Right $ D (op x y)
numericOp op (F x) (F y) = Right $ F (realToFrac $ op (realToFrac x) (realToFrac y))
numericOp op (F x) (N y) = Right $ F (realToFrac $ op (realToFrac x) (toDouble y))
numericOp op (F x) (D y) = Right $ F (realToFrac $ op (realToFrac x) y)
numericOp op (N x) (F y) = Right $ F (realToFrac $ op (toDouble x) (realToFrac y))
numericOp op (D x) (F y) = Right $ F (realToFrac $ op x (realToFrac y))
numericOp _ _ _ = Left "Invalid numeric op"


operatorAdd :: [Value] -> Either String Value
operatorAdd (y : x : _) = numericOp (+) x y
operatorAdd _ = Left "expects two number"

operatorSub :: [Value] -> Either String Value
operatorSub (y : x : _) = numericOp (-) x y
operatorSub _ = Left "expects two number"

operatorMul :: [Value] -> Either String Value
operatorMul (y : x : _) = numericOp (*) x y
operatorMul _ = Left "expects two number"

operatorDiv :: [Value] -> Either String Value
operatorDiv (y : x : _) =
    case y of
        N y' | y' == 0 -> Left "division by zero"
        D y' | y' == 0.0 -> Left "division by zero"
        F y' | y' == 0.0 -> Left "division by zero"
        _ -> numericOp (/) x y
operatorDiv _ = Left "expects two number"

operatorMod :: [Value] -> Either String Value
operatorMod (N y : N x:_)
    | y == 0 = Left "modulo by zero"
    | otherwise = Right $ N (x `mod` y)
operatorMod _ = Left "xpects two int"
