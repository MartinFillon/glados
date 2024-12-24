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
import Control.Monad.IO.Class (liftIO)

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
    | Op ([Value] -> IO (Either String Value))
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
    | Jump Int
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

operatorAdd :: [Value] -> IO (Either String Value)
operatorAdd (y : x : _) = return $ numericOp (+) x y
operatorAdd _ = return $ Left "expects two number"

operatorSub :: [Value] -> IO (Either String Value)
operatorSub (y : x : _) = return $ numericOp (-) x y
operatorSub _ = return $ Left "expects two number"

operatorMul :: [Value] -> IO (Either String Value)
operatorMul (y : x : _) = return $ numericOp (*) x y
operatorMul _ = return $ Left "expects two number"

operatorDiv :: [Value] -> IO (Either String Value)
operatorDiv (y : x : _) =
    case y of
        N y' | y' == 0 -> return $ Left "division by zero"
        D y' | y' == 0.0 -> return $ Left "division by zero"
        F y' | y' == 0.0 -> return $ Left "division by zero"
        _ -> return $ numericOp (/) x y
operatorDiv _ = return $ Left "expects two number"

operatorMod :: [Value] -> IO (Either String Value)
operatorMod (N y : N x:_)
    | y == 0 = return $ Left "modulo by zero"
    | otherwise = return $ Right $ N (x `mod` y)
operatorMod _ = return $ Left "xpects two int"

operatorEq :: [Value] -> IO (Either String Value)
operatorEq (x : y : _) = return $ Right $ B (x == y)
operatorEq _ = return $ Left "expects two value"

operatorLt :: [Value] -> IO (Either String Value)
operatorLt (y : x : _) =
    case (x, y) of
        (N a, N b) -> return $ Right $ B (toDouble a < toDouble b)
        (N a, D b) -> return $ Right $ B (toDouble a < b)
        (N a, F b) -> return $ Right $ B (toDouble a < realToFrac b)
        (D a, N b) -> return $ Right $ B (a < toDouble b)
        (D a, D b) -> return $ Right $ B (a < b)
        (D a, F b) -> return $ Right $ B (a < realToFrac b)
        (F a, N b) -> return $ Right $ B (realToFrac a < toDouble b)
        (F a, D b) -> return $ Right $ B (realToFrac a < b)
        (F a, F b) -> return $ Right $ B (a < b)
        _ -> return $ Left "expects two number"
operatorLt _ = return $ Left "expects two number"

operatorGt :: [Value] -> IO (Either String Value)
operatorGt (y : x : _) =
    case (x, y) of
        (N a, N b) -> return $ Right $ B (toDouble a > toDouble b)
        (N a, D b) -> return $ Right $ B (toDouble a > b)
        (N a, F b) -> return $ Right $ B (toDouble a > realToFrac b)
        (D a, N b) -> return $ Right $ B (a > toDouble b)
        (D a, D b) -> return $ Right $ B (a > b)
        (D a, F b) -> return $ Right $ B (a > realToFrac b)
        (F a, N b) -> return $ Right $ B (realToFrac a > toDouble b)
        (F a, D b) -> return $ Right $ B (realToFrac a > b)
        (F a, F b) -> return $ Right $ B (a > b)
        _ -> return $ Left "expects two number"
operatorGt _ = return $ Left "expects two number"

operatorAnd :: [Value] -> IO (Either String Value)
operatorAnd (B y : B x : _) = return $ Right $ B (x && y)
operatorAnd _ = return $ Left "And expects two bool"

operatorOr :: [Value] -> IO (Either String Value)
operatorOr (B y : B x : _) = return $ Right $ B (x || y)
operatorOr _ = return $ Left "Or expects two booleans"

operatorPrint :: [Value] -> IO (Either String Value)
operatorPrint (S s : _) = do
    liftIO $ putStrLn s
    return $ Right $ N (fromIntegral (length s))
operatorPrint (val : _) = do
    liftIO $ putStrLn (show val)
    return $ Right $ N (fromIntegral (length (show val)))
operatorPrint _ = return $ Left "expects one val"

operatorGet :: [Value] -> IO (Either String Value)
operatorGet (N idx : L lst : _)
    | idx >= 0 && idx < fromIntegral (length lst) = return $ Right $ lst !! fromIntegral idx
    | otherwise = return $ Left "Index out of bound"
operatorGet _ = return $ Left "expects a list and an integer index"

operatorSet :: [Value] -> IO (Either String Value)
operatorSet (val : N idx : L lst : _)
    | idx >= 0 && idx < fromIntegral (length lst) = return $ Right $ L (take (fromIntegral idx) lst ++ [val] ++ drop (fromIntegral idx + 1) lst)
    | otherwise = return $ Left "Index out of bound"
operatorSet _ = return $ Left "expects a list, an integer index, and a value"

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

exec :: Memory -> Args -> Insts -> Stack -> IO (Either String Value)
exec _ _ [] [] = return $ Left "No value on stack"
exec _ _ [] (v : _) = return $ Right v
exec mem args (Noop : is) stack = exec mem args is stack
exec mem args (Push val : is) stack = exec mem args is (val : stack)
exec mem args (PushArg i : is) stack
    | i >= 0 && i < length args = exec mem args is (args !! i : stack)
    | otherwise = return $ Left "PushArg index out of range"

exec mem args (Call key : is) stack =
    case Map.lookup key mem of
        Just (Op f) -> do
            result <- f stack
            case result of
                Right res -> exec mem args is (res : drop 2 stack)
                Left err -> return $ Left err
        Just (Bi code) -> do
            res <- exec mem stack code []
            case res of
                Right val -> exec mem args is (val : stack)
                Left err -> return $ Left err
        _ -> return $ Left ("Call on invalid or missing key: " ++ key)

exec mem args (JumpIfFalse n : is) (B b : stack)
    | not b = exec mem args (drop n is) stack
    | otherwise = exec mem args is stack

exec mem args (Jump n : is) stack = exec mem args (drop n is) stack

exec _ _ (JumpIfFalse _ : _) _ = return $ Left "JumpIfFalse needs a bool on the stack"
exec _ _ (Ret : _) [] = return $ Left "Error Stack on Ret"
exec _ _ (Ret : _) (top : _) = return $ Right top
