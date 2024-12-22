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

operatorEq :: [Value] -> Either String Value
operatorEq (x : y : _) = Right $ B (x == y)
operatorEq _ = Left "expects two value"

operatorLt :: [Value] -> Either String Value
operatorLt (y : x : _) =
    case (x, y) of
        (N a, N b) -> Right $ B (toDouble a < toDouble b)
        (N a, D b) -> Right $ B (toDouble a < b)
        (N a, F b) -> Right $ B (toDouble a < realToFrac b)
        (D a, N b) -> Right $ B (a < toDouble b)
        (D a, D b) -> Right $ B (a < b)
        (D a, F b) -> Right $ B (a < realToFrac b)
        (F a, N b) -> Right $ B (realToFrac a < toDouble b)
        (F a, D b) -> Right $ B (realToFrac a < b)
        (F a, F b) -> Right $ B (a < b)
        _ -> Left "expects two number"
operatorLt _ = Left "expects two number"

operatorGt :: [Value] -> Either String Value
operatorGt (y : x : _) =
    case (x, y) of
        (N a, N b) -> Right $ B (toDouble a > toDouble b)
        (N a, D b) -> Right $ B (toDouble a > b)
        (N a, F b) -> Right $ B (toDouble a > realToFrac b)
        (D a, N b) -> Right $ B (a > toDouble b)
        (D a, D b) -> Right $ B (a > b)
        (D a, F b) -> Right $ B (a > realToFrac b)
        (F a, N b) -> Right $ B (realToFrac a > toDouble b)
        (F a, D b) -> Right $ B (realToFrac a > b)
        (F a, F b) -> Right $ B (a > b)
        _ -> Left "expects two number"
operatorGt _ = Left "expects two number"

operatorAnd :: [Value] -> Either String Value
operatorAnd (B y : B x : _) = Right $ B (x && y)
operatorAnd _ = Left "And expects two bool"

operatorOr :: [Value] -> Either String Value
operatorOr (B y : B x : _) = Right $ B (x || y)
operatorOr _ = Left "Or expects two booleans"

operatorPrint :: [Value] -> Either String Value
operatorPrint (S s : _) = Right $ N (fromIntegral (length s))
operatorPrint (val : _) = Right $ N (fromIntegral (length (show val)))
operatorPrint _ = Left "expects one val"

operatorGet :: [Value] -> Either String Value
operatorGet (N idx : L lst : _)
    | idx >= 0 && idx < fromIntegral (length lst) = Right $ lst !! fromIntegral idx
    | otherwise = Left "Index out of bound"
operatorGet _ = Left "expects a list and an integer index"

operatorSet :: [Value] -> Either String Value
operatorSet (val : N idx : L lst : _)
    | idx >= 0 && idx < fromIntegral (length lst) = Right $ L (take (fromIntegral idx) lst ++ [val] ++ drop (fromIntegral idx + 1) lst)
    | otherwise = Left "Index out of bound"
operatorSet _ = Left "expects a list, an integer index, and a value"

initialMemory :: Memory
initialMemory = Map.fromList
    [
        ("add", Op operatorAdd),
        ("sub", Op operatorSub),
        ("mul", Op operatorMul),
        ("div", Op operatorDiv),
        ("mod", Op operatorMod),
        ("eq", Op operatorEq),
        ("less", Op operatorLt),
        ("greater", Op operatorGt),
        ("and", Op operatorAnd),
        ("or", Op operatorOr),
        ("print", Op operatorPrint),
        ("get", Op operatorGet),
        ("set", Op operatorSet)
    ]

exec :: Memory -> Args -> Insts -> Stack -> Either String Value
exec _ _ [] [] = Left "No value on stack"
exec _ _ [] (v : _) = Right v
exec mem args (Noop : is) stack = exec mem args is stack
exec mem args (Push val : is) stack = exec mem args is (val : stack)
exec mem args (PushArg i : is) stack
    | i >= 0 && i < length args = exec mem args is (args !! i : stack)
    | otherwise = Left "PushArg index out of range"

exec mem args (Call key : is) stack =
    case Map.lookup key mem of
        Just (Op f) -> case f stack of
            Right result -> exec mem args is (result : drop 2 stack)
            Left err -> Left err
        Just (Bi code) -> case exec mem stack code [] of
            Right res -> exec mem args is (res : stack)
            Left err -> Left err
        _ -> Left ("Call on invalid or missing key: " ++ key)

exec mem args (JumpIfFalse n : is) (B b : stack)
    | not b = exec mem args (drop n is) stack
    | otherwise = exec mem args is stack

exec _ _ (JumpIfFalse _ : _) _ = Left "JumpIfFalse needs a bool on the stack"
exec _ _ (Ret : _) [] = Left "Error Stack on Ret"
exec _ _ (Ret : _) (top : _) = Right top

