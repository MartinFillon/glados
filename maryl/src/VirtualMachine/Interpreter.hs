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

