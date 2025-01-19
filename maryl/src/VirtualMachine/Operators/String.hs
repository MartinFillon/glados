{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- String
-}

module VirtualMachine.Operators.String (
    strcat,
    strlen,
    substr,
    strcmp,
) where

import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState)

strcat :: [Value] -> VmState [Value]
strcat (S y : S x : xs) = return $ S (x ++ y) : xs
strcat _ = fail "strcat expects two strings"

strlen :: [Value] -> VmState [Value]
strlen (S x : xs) = return $ N (fromIntegral $ length x) : xs
strlen _ = fail "strlen expects a string"

substr :: [Value] -> VmState [Value]
substr (N len : N start : S str : xs)
    | start < 0 || len < 0 || start > fromIntegral (length str) = fail "substr: invalid range"
    | otherwise = return (S (take (fromIntegral len) $ drop (fromIntegral start) str) : xs)
substr _ = fail "substr expects a string and two numbers"

strcmp :: [Value] -> VmState [Value]
strcmp (S y : S x : xs) =
    return $
        N
            ( case compare x y of
                LT -> -1
                EQ -> 0
                GT -> 1
            )
            : xs
strcmp _ = fail "strcmp expects two strings"
