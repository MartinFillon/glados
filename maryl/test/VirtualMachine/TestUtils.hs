{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- TestUtils
-}

module VirtualMachine.TestUtils (execTest, execTest', constIO, userError') where

import Control.Exception (IOException)
import Control.Monad.State (evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.IO.Exception (IOException (IOError))
import VirtualMachine.Instructions (
    Instruction,
    Value,
 )
import VirtualMachine.Interpreter (exec)
import VirtualMachine.Operators (operators)
import VirtualMachine.State (V (..), initialMemory, initialState)

execTest :: [Instruction] -> IO Value
execTest is = evalStateT exec (initialState is (initialMemory $ Map.fromList operators) [])

execTest' :: [Instruction] -> Map String V -> IO Value
execTest' is m = evalStateT exec (initialState is (initialMemory m) [])

constIO :: IOException -> Bool
constIO = const True

userError' :: String -> IOException -> Bool
userError' s (IOError _ _ _ s' _ _) = s == s'
