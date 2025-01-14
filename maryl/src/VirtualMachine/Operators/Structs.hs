{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Structs
-}

module VirtualMachine.Operators.Structs (getStructValue, setStructValue) where

import qualified Data.Map as Map
import VirtualMachine.Instructions (Value (..))
import VirtualMachine.State (VmState)

getStructValue' :: String -> Maybe Value -> VmState Value
getStructValue' _ (Just a) = pure a
getStructValue' s Nothing = fail s

getStructValue :: [Value] -> VmState [Value]
getStructValue (S field : s@(St struct) : xs) =
    (: s : xs)
        <$> getStructValue'
            (field ++ ": No such field in struct: " ++ show s)
            (Map.lookup field struct)
getStructValue _ = fail "The getField function takes a struct followed by a string as param."

setStructValue :: [Value] -> VmState [Value]
setStructValue (val : S field : (St struct) : xs)
    | Map.member field struct = pure (St (Map.insert field val struct) : xs)
    | otherwise = fail $ field ++ ": field not in struct: " ++ show (St struct)
setStructValue _ = fail "The setField function takes"
