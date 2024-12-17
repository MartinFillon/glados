{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}

module VirtualMachine.Instructions (Insts (..), exec, execOp, Val (..)) where

data Val
    = N Int
    | B Bool
    | S String
    | I [Insts]
    deriving (Show)

data Insts
    = Push Val
    | Ret
    | Call
    | JumpF Int
    | PushArg Int
    | PushFromEnv String
    deriving (Show)

type Env = [(String, Val)]

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

getArg :: Int -> [Val] -> Either String Val
getArg _ [] = Left "Arg not found"
getArg 0 (x : _) = Right x
getArg n (_ : xs) = getArg (n - 1) xs

getEnv :: String -> Env -> Either String Val
getEnv _ [] = Left "Arg not found"
getEnv s ((x, v) : xs)
    | s == x = Right v
    | otherwise = getEnv s xs

exec :: Env -> [Val] -> [Insts] -> Stack -> Either String Val
exec _ _ (Ret : _) (x : _) = Right x
exec e a (Push x : xs) s = exec e a xs (x : s)
exec e a (Call : xs) (S f : s) = execOp f s >>= exec e a xs
exec e a (Call : xs) (I i : s) = exec e s i [] >>= (\r -> exec e a xs (r : s))
exec e a (PushArg n : xs) s = getArg n a >>= (\x -> exec e a xs (x : s))
exec e a (JumpF n : xs) (B False : s) = exec e a (skip xs n) s
exec e a (JumpF _ : xs) (B True : s) = exec e a xs s
exec e a (PushFromEnv n : xs) s = getEnv n e >>= (\r -> exec e a xs (r : s))
exec _ _ _ _ = Left "Missing infos"
