{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}

module VirtualMachine.Instructions (Insts (..), exec, execOp) where

type Op = String

data Insts
    = Push Int
    | Ret
    | Call Op
    deriving (Show)

type Stack = [Int]

execOp :: String -> Stack -> Either String Stack
execOp "Add" (x : y : stack) = Right ((+) x y : stack)
execOp "Add" _ = Left "Add: Not enough args"
execOp "Mul" (x : y : stack) = Right ((*) x y : stack)
execOp "Mul" _ = Left "Mul: Not enough args"
execOp "Sub" (x : y : stack) = Right ((-) x y : stack)
execOp "Sub" _ = Left "Sub: Not enough args"
execOp "Div" (_ : 0 : _) = Left "Div: Division by zero"
execOp "Div" (x : y : stack) = Right (div x y : stack)
execOp "Div" _ = Left "Div: Not enough args"
execOp x _ = Left (x ++ ": Unknown")

exec :: [Insts] -> Stack -> Either String Int
exec (Ret : _) (x : _) = Right x
exec (Push x : xs) s = exec xs (x : s)
exec (Call x : xs) s = execOp x s >>= exec xs
exec _ _ = Left "Missing infos"
