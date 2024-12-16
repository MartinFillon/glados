{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module VirtualMachine.Instructions (push, pop, exec, Insts (..)) where

import Control.Arrow (ArrowChoice (right))
import Debug.Trace (trace, traceShow, traceShowId)

data Insts = Push Int | Ret deriving (Show)

newtype Stack state a = Stack
    { run :: state -> Either String (a, state)
    }

instance Functor (Stack st) where
    fmap :: (a -> b) -> Stack st a -> Stack st b
    fmap f (Stack r) =
        Stack
            ( \st -> case r st of
                Right (res, state) -> Right (f res, state)
                (Left e) -> Left e
            )

instance Applicative (Stack st) where
    pure :: a -> Stack st a
    pure x = Stack (\st -> Right (x, st))
    (<*>) :: Stack st (a -> b) -> Stack st a -> Stack st b
    (<*>) (Stack r1) (Stack r2) =
        Stack
            ( \st -> case r2 st of
                Right (res1, state) -> case r1 state of
                    Right (res2, state') -> Right (res2 res1, state')
                    Left e -> Left e
                Left e -> Left e
            )

-- let (result, state) = r st
-- in run (f result) state

instance Monad (Stack st) where
    (>>=) :: Stack st a -> (a -> Stack st b) -> Stack st b
    (>>=) (Stack r) f =
        Stack
            ( \st -> case r st of
                Right (res, state) -> run (f res) state
                Left e -> Left e
            )

push :: Int -> Stack [Int] Int
push x =
    Stack
        ( \st -> Right (0, x : st)
        )

pop :: Stack [Int] Int
pop =
    Stack
        ( \st -> case st of
            (x : xs) -> Right (x, xs)
            _ -> Left "No value available"
        )

getInstExec :: Insts -> Stack [Int] Int
getInstExec (Push x) = push x
getInstExec Ret = pop

exec' :: Stack [Int] Int -> Either String Int
exec' (Stack r) = right fst (r [])

exec'' :: [Stack [Int] Int] -> Stack [Int] Int
exec'' [] = return 0
exec'' [x] = x
exec'' (x : xs) = x >> exec'' xs

exec :: [Insts] -> Either String Int
exec x = exec' $ exec'' (map getInstExec x)
