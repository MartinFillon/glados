{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}
{-# LANGUAGE InstanceSigs #-}

module VirtualMachine.Instructions (
    Insts (..),
    Val (..),
    Op (..),
    push,
    call,
    ret,
    noop,
    pushArg,
    jumpf,
) where

import Data.Char (ord)
import Data.Int (Int64)
import Data.Word (Word8)
import VirtualMachine.Bytes (ToBytes (..))

data Op = Op
    { code :: Word8,
      name :: String,
      inst :: Insts
    }
    deriving (Show, Eq)

data Val
    = N Int64
    | B Bool
    | C Char
    | L [Val]
    deriving (Show, Eq)

-- d64
-- lc5char\0

instance ToBytes Val where
    toByte :: Val -> [Word8]
    toByte (N n) = fromIntegral (ord 'i') : toByte n
    toByte (B False) = fromIntegral (ord 'b') : [0]
    toByte (B True) = fromIntegral (ord 'b') : [1]
    toByte (C c) = fromIntegral (ord 'c') : [fromIntegral (ord c)]
    toByte (L s@(N _ : _)) =
        fromIntegral (ord 'l')
            : fromIntegral (ord 'i')
            : toByte (length s)
            ++ concatMap toByte s
    toByte (L s@(B _ : _)) =
        fromIntegral (ord 'l')
            : fromIntegral (ord 'b')
            : toByte (length s)
            ++ concatMap toByte s
    toByte (L s@(C _ : _)) =
        fromIntegral (ord 'l')
            : fromIntegral (ord 'c')
            : toByte (length s)
            ++ concatMap toByte s
    toByte _ = []

data Insts
    = Push Val
    | Ret
    | Call
    | JumpF Int64
    | PushArg Int64
    | Noop
    deriving (Show, Eq)

instance ToBytes Insts where
    toByte :: Insts -> [Word8]
    toByte Noop = [code noop]
    toByte Ret = [code ret]
    toByte Call = [code call]
    toByte (Push x) = code (push x) : toByte x
    toByte (PushArg x) = code (pushArg x) : toByte x
    toByte (JumpF f) = code (jumpf f) : toByte f

type Env = [(String, Val)]

type Stack = [Val]

noop :: Op
noop = Op 0 "Noop" Noop

call :: Op
call = Op 1 "Call" Call

push :: Val -> Op
push x = Op 2 "Push" (Push x)

ret :: Op
ret = Op 3 "Ret" Ret

jumpf :: Int64 -> Op
jumpf x = Op 4 "JumpF" (JumpF x)

pushArg :: Int64 -> Op
pushArg x = Op 5 "PushArg" (PushArg x)

-- execOp :: String -> Stack -> Either String Stack
-- execOp "Add" (N x : N y : stack) = Right (N ((+) x y) : stack)
-- execOp "Add" _ = Left "Add: Bad args"
-- execOp "Mul" (N x : N y : stack) = Right (N ((*) x y) : stack)
-- execOp "Mul" _ = Left "Mul: Bad args"
-- execOp "Sub" (N x : N y : stack) = Right (N ((-) x y) : stack)
-- execOp "Sub" _ = Left "Sub: Bad args"
-- execOp "Div" (_ : N 0 : _) = Left "Div: Division by zero"
-- execOp "Div" (N x : N y : stack) = Right (N (div x y) : stack)
-- execOp "Div" _ = Left "Div: Bad args"
-- execOp "Eq" (N x : N y : stack) = Right (B ((==) x y) : stack)
-- execOp "Eq" _ = Left "Eq: Bad args"
-- execOp "Less" (N x : N y : stack) = Right (B ((<) x y) : stack)
-- execOp "Less" _ = Left "Less: Bad args"
-- execOp x _ = Left (x ++ ": Unknown")

-- skip :: (Eq x, Num x) => [a] -> x -> [a]
-- skip [] _ = []
-- skip x 0 = x
-- skip (_ : xs) n = skip xs (n - 1)

-- getArg :: (Eq a, Num a) => a -> [Val] -> Either String Val
-- getArg _ [] = Left "Arg not found"
-- getArg 0 (x : _) = Right x
-- getArg n (_ : xs) = getArg (n - 1) xs

-- getEnv :: String -> Env -> Either String Val
-- getEnv _ [] = Left "Arg not found"
-- getEnv s ((x, v) : xs)
--     | s == x = Right v
--     | otherwise = getEnv s xs

-- exec :: Env -> [Val] -> [Insts] -> Stack -> Either String Val
-- exec _ _ (Ret : _) (x : _) = Right x
-- exec e a (Push x : xs) s = exec e a xs (x : s)
-- exec e a (Call : xs) (S f : s) = execOp f s >>= exec e a xs
-- exec e a (Call : xs) (I i : s) = exec e s i [] >>= (\r -> exec e a xs (r : s))
-- exec e a (PushArg n : xs) s = getArg n a >>= (\x -> exec e a xs (x : s))
-- exec e a (JumpF n : xs) (B False : s) = exec e a (skip xs n) s
-- exec e a (JumpF _ : xs) (B True : s) = exec e a xs s
-- exec _ _ _ _ = Left "Missing infos"
