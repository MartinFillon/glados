{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}

module VirtualMachine.Instructions (Insts (..), exec, execOp, Val (..)) where

type Op = String

data Val
    = N Int
    | B Bool
    deriving (Show)

data Insts
    = Push Val
    | Ret
    | Call Op
    | JumpF Int
    | PushArg Int
    deriving (Show)

type Stack = [Val]

execOp :: String -> Stack -> Either String Stack
execOp "Add" (N x : N y : stack) = Right (N ((+) x y) : stack)
execOp "Add" _ = Left "Add: Bad args"
execOp "Mul" (N x : N y : stack) = Right (N ((*) x y) : stack)
execOp "Mul" _ = Left "Mul: Bad args"
execOp "Sub" (N x : N y : stack) = Right (N ((-) x y) : stack)
execOp "Sub" _ = Left "Sub: Bad args"
execOp "Div" (_ : N 0 : _) = Left "Div: Division by zero"
execOp "Div" (N x : N y : stack) = Right (N (div x y) : stack)
execOp "Div" _ = Left "Div: Bad args"
execOp "Eq" (N x : N y : stack) = Right (B ((==) x y) : stack)
execOp "Eq" _ = Left "Eq: Bad args"
execOp "Less" (N x : N y : stack) = Right (B ((<) x y) : stack)
execOp "Less" _ = Left "Less: Bad args"
execOp x _ = Left (x ++ ": Unknown")

skip :: [a] -> Int -> [a]
skip [] _ = []
skip x 0 = x
skip (_ : xs) n = skip xs (n - 1)

getArg :: Int -> [Int] -> Either String Val
getArg _ [] = Left "Arg not found"
getArg 0 (x : _) = Right (N x)
getArg n (_ : xs) = getArg (n - 1) xs

exec :: [Int] -> [Insts] -> Stack -> Either String Val
exec _ (Ret : _) (x : _) = Right x
exec a (Push x : xs) s = exec a xs (x : s)
exec a (Call x : xs) s = execOp x s >>= exec a xs
exec a (PushArg n : xs) s = getArg n a >>= (\x -> exec a xs (x : s))
exec a (JumpF n : xs) (B False : s) = exec a (skip xs n) s
exec a (JumpF _ : xs) (B True : s) = exec a xs s
exec _ _ _ = Left "Missing infos"
