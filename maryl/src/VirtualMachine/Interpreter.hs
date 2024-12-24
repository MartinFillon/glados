{-
-- EPITECH PROJECT, 2024
-- inst [WSL: Ubuntu]
-- File description:
-- interpreter
-}
{-# LANGUAGE InstanceSigs #-}

module VirtualMachine.Interpreter (
    Numeric (..),
    Stack,
    Insts,
    Args,
    Memory,
    initialMemory,
    exec,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import VirtualMachine.Instructions (Inst (..), Value (..))

class Numeric a where
    toDouble :: a -> Double
    fromDouble :: Double -> Either String a

instance Numeric Int64 where
    toDouble :: Int64 -> Double
    toDouble = fromIntegral
    fromDouble :: Double -> Either String Int64
    fromDouble d =
        if (fromIntegral (round d :: Int64) :: Double) == d
            then Right (round d :: Int64)
            else Left "Cant convert to Int64 without loss"

instance Numeric Double where
    toDouble :: Double -> Double
    toDouble = id
    fromDouble :: Double -> Either String Double
    fromDouble = Right

type Stack = [Value]
type Insts = [Inst]
type Args = [Value]

------------
-- Memory --
------------

data Memory
    = M (Map String Value) (Map String (Stack -> IO (Either String Value)))

initialMemory :: Memory
initialMemory = M operators ioOperators

getFromMem ::
    Memory -> String -> Maybe (Either Value (Stack -> IO (Either String Value)))
getFromMem (M o io) s = case Map.lookup s o of
    Just x -> Just $ Left x
    Nothing -> Map.lookup s io <&> Right

-------------------------
-- operator operations --
-------------------------

numericOp ::
    (Double -> Double -> Double) -> Value -> Value -> Either String Value
numericOp op (N x) (N y) = case fromDouble (op (toDouble x) (toDouble y)) of
    Right n -> Right $ N n
    Left _ -> Right $ D (op (toDouble x) (toDouble y))
numericOp op (N x) (D y) = Right $ D (op (toDouble x) y)
numericOp op (D x) (N y) = Right $ D (op x (toDouble y))
numericOp op (D x) (D y) = Right $ D (op x y)
numericOp _ _ _ = Left "Invalid numeric op"

operatorAdd :: Stack -> Either String Value
operatorAdd (y : x : _) = numericOp (+) x y
operatorAdd _ = Left "expects two number"

operatorSub :: Stack -> Either String Value
operatorSub (y : x : _) = numericOp (-) x y
operatorSub _ = Left "expects two number"

operatorMul :: Stack -> Either String Value
operatorMul (y : x : _) = numericOp (*) x y
operatorMul _ = Left "expects two number"

operatorDiv :: Stack -> Either String Value
operatorDiv (y : x : _) =
    case y of
        N y' | y' == 0 -> Left "division by zero"
        D y' | y' == 0.0 -> Left "division by zero"
        _ -> numericOp (/) x y
operatorDiv _ = Left "expects two number"

operatorMod :: Stack -> Either String Value
operatorMod (N y : N x : _)
    | y == 0 = Left "modulo by zero"
    | otherwise = Right $ N (x `mod` y)
operatorMod _ = Left "xpects two int"

operatorEq :: Stack -> Either String Value
operatorEq (x : y : _) = Right $ B (x == y)
operatorEq _ = Left "expects two value"

operatorLt :: Stack -> Either String Value
operatorLt (y : x : _) =
    case (x, y) of
        (N a, N b) -> Right $ B (toDouble a < toDouble b)
        (N a, D b) -> Right $ B (toDouble a < b)
        (D a, N b) -> Right $ B (a < toDouble b)
        (D a, D b) -> Right $ B (a < b)
        _ -> Left "expects two number"
operatorLt _ = Left "expects two number"

operatorGt :: Stack -> Either String Value
operatorGt (y : x : _) =
    case (x, y) of
        (N a, N b) -> Right $ B (toDouble a > toDouble b)
        (N a, D b) -> Right $ B (toDouble a > b)
        (D a, N b) -> Right $ B (a > toDouble b)
        (D a, D b) -> Right $ B (a > b)
        _ -> Left "expects two number"
operatorGt _ = Left "expects two number"

operatorAnd :: Stack -> Either String Value
operatorAnd (B y : B x : _) = Right $ B (x && y)
operatorAnd _ = Left "And expects two bool"

operatorOr :: Stack -> Either String Value
operatorOr (B y : B x : _) = Right $ B (x || y)
operatorOr _ = Left "Or expects two booleans"

operatorPrint :: Stack -> IO (Either String Value)
operatorPrint (S s : _) = do
    liftIO $ putStrLn s
    return $ Right $ N (fromIntegral (length s))
operatorPrint (val : _) = do
    liftIO $ print val
    return $ Right $ N (fromIntegral (length (show val)))
operatorPrint _ = return $ Left "expects one val"

operatorGet :: Stack -> Either String Value
operatorGet (N idx : L lst : _)
    | idx >= 0 && idx < fromIntegral (length lst) = Right $ lst !! fromIntegral idx
    | otherwise = Left "Index out of bound"
operatorGet _ = Left "expects a list and an integer index"

operatorSet :: Stack -> Either String Value
operatorSet (val : N idx : L lst : _)
    | idx >= 0 && idx < fromIntegral (length lst) =
        Right $
            L
                (take (fromIntegral idx) lst ++ [val] ++ drop (fromIntegral idx + 1) lst)
    | otherwise = Left "Index out of bound"
operatorSet _ = Left "expects a list, an integer index, and a value"

operators :: Map String Value
operators =
    Map.fromList
        [ ("add", Op operatorAdd),
          ("sub", Op operatorSub),
          ("mul", Op operatorMul),
          ("div", Op operatorDiv),
          ("mod", Op operatorMod),
          ("eq", Op operatorEq),
          ("less", Op operatorLt),
          ("greater", Op operatorGt),
          ("and", Op operatorAnd),
          ("or", Op operatorOr),
          ("get", Op operatorGet),
          ("set", Op operatorSet)
        ]

ioOperators :: Map String (Stack -> IO (Either String Value))
ioOperators = Map.fromList [("print", operatorPrint)]

-- execCall :: Memory

exec :: Memory -> Args -> Insts -> Stack -> IO (Either String Value)
exec _ _ [] [] = return $ Left "No value on stack"
exec _ _ [] (v : _) = return $ Right v
exec mem args (Noop : is) stack = exec mem args is stack
exec mem args (Push val : is) stack = exec mem args is (val : stack)
exec mem args (PushArg i : is) stack
    | i >= 0 && i < length args = exec mem args is (args !! i : stack)
    | otherwise = return $ Left "PushArg index out of range"
exec mem args (Call key : is) stack =
    case getFromMem mem key of
        Just (Left (Op f)) -> do
            case f stack of
                Right res -> exec mem args is (res : drop 2 stack)
                Left err -> return $ Left err
        Just (Left (Bi code)) -> do
            res <- exec mem stack code []
            case res of
                Right val -> exec mem args is (val : stack)
                Left err -> return $ Left err
        Just (Right f) -> do
            r <- f stack
            case r of
                Right val -> exec mem args is (val : stack)
                Left err -> return $ Left err
        _ -> return $ Left ("Call on invalid or missing key: " ++ key)
exec mem args (JumpIfFalse (Left n) : is) (B b : stack)
    | not b = exec mem args (drop n is) stack
    | otherwise = exec mem args is stack
exec mem args (Jump (Left n) : is) stack = exec mem args (drop n is) stack
exec _ _ (JumpIfFalse _ : _) _ = return $ Left "JumpIfFalse needs a bool on the stack"
exec _ _ (Ret : _) [] = return $ Left "Error Stack on Ret"
exec _ _ (Ret : _) (top : _) = return $ Right top
exec _ _ _ _ = return $ Left "Not handled"
