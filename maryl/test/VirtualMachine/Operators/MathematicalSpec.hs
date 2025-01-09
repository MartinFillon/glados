{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- MathematicalSpec
-}

module VirtualMachine.Operators.MathematicalSpec (spec) where

import Control.Exception (IOException)
import Control.Monad.State (evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Error (isDoesNotExistError)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import VirtualMachine.Instructions (
    Instruction,
    Value (..),
    call,
    get,
    jump,
    jumpf,
    load,
    push,
    pushArg,
    ret,
 )
import VirtualMachine.Interpreter (exec)
import VirtualMachine.Operators (operators)
import VirtualMachine.State (V (..), initialState)
import VirtualMachine.TestUtils (execTest)

spec :: Spec
spec =
    describe "Mathematical operators testing" $ do
        it "should execute addition 10 + 30 + 20" $
            do
                execTest
                    [ push Nothing (N 10),
                      push Nothing (N 30),
                      call Nothing "add",
                      push Nothing (N 20),
                      call Nothing "add",
                      ret Nothing
                    ]
                    `shouldReturn` N 60
        it
            "should execute mixed addition (5 + 3.14)"
            $ do
                execTest
                    [ push Nothing (N 5),
                      push Nothing (D 3.14),
                      call Nothing "add",
                      ret Nothing
                    ]
                    `shouldReturn` D 8.14
        it
            "should execute mixed multiplication (2.5 * 3)"
            $ do
                execTest
                    [ push Nothing (D 2.5),
                      push Nothing (N 3),
                      call Nothing "mul",
                      ret Nothing
                    ]
                `shouldReturn` D
                    7.5
        it "should execute subtraction 30 - 10.5" $
            do
                execTest
                    [ push Nothing (D 30),
                      push Nothing (D 10.5),
                      call Nothing "sub",
                      ret Nothing
                    ]
                    `shouldReturn` D 19.5
        it "should execute division 40 / 8" $
            do
                execTest
                    [ push Nothing (N 40),
                      push Nothing (N 8),
                      call Nothing "div",
                      ret Nothing
                    ]
                `shouldReturn` N
                    5
        it "should execute mixed division (5.0 / 2)" $
            do
                execTest
                    [push Nothing (D 5.0), push Nothing (N 2), call Nothing "div", ret Nothing]
                `shouldReturn` D
                    2.5
        it "should execute modulo 10 % 3" $
            do
                execTest
                    [push Nothing (N 10), push Nothing (N 3), call Nothing "mod", ret Nothing]
                `shouldReturn` N 1

        it "should execute addition 10 + 30 + 20 with mixed types" $
            do
                execTest
                    [ push Nothing (N 10),
                      push Nothing (D 30),
                      call Nothing "add",
                      push Nothing (N 20),
                      call Nothing "add",
                      ret Nothing
                    ]
                `shouldReturn` D 60

        it "should execute subtraction 30 - 10" $
            do
                execTest
                    [push Nothing (N 30), push Nothing (N 10), call Nothing "sub", ret Nothing]
                `shouldReturn` N 20

        it "should execute multiplication 6 * 7" $
            do
                execTest
                    [push Nothing (N 6), push Nothing (N 7), call Nothing "mul", ret Nothing]
                `shouldReturn` N 42

        it "should execute division 40 / 8" $
            do
                execTest
                    [push Nothing (N 40), push Nothing (N 8), call Nothing "div", ret Nothing]
                `shouldReturn` N 5

        it "should execute modulo 10 % 3" $
            do
                execTest
                    [push Nothing (N 10), push Nothing (N 3), call Nothing "mod", ret Nothing]
                `shouldReturn` N 1
