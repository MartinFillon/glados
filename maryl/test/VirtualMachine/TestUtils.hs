{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- TestUtils
-}

module VirtualMachine.TestUtils (execTest, execTest', constIO) where

import Control.Exception (IOException)
import Control.Monad.State (evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import VirtualMachine.Instructions (
    Instruction,
    Value,
 )
import VirtualMachine.Interpreter (exec)
import VirtualMachine.Operators (operators)
import VirtualMachine.State (V (..), initialState)

execTest :: [Instruction] -> IO Value
execTest is = evalStateT exec (initialState is (Map.fromList operators) [])

execTest' :: [Instruction] -> Map String V -> IO Value
execTest' is m = evalStateT exec (initialState is m [])

constIO :: IOException -> Bool
constIO = const True
