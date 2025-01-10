{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- LogicSpec
-}

module VirtualMachine.Operators.LogicSpec (spec) where

import Control.Exception (IOException)
import Control.Monad.State (evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
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
import VirtualMachine.TestUtils (constIO, execTest)

notEnoughArg :: String -> Spec
notEnoughArg s = it ("should fail on " ++ s ++ " cause not enough arguments") $ do
    execTest
        [push Nothing (N 3), call Nothing s, ret Nothing]
        `shouldThrow` constIO

spec :: Spec
spec = do
    describe "Logic Operators Interpretation Spec" $ do
        it "should execute equality Hello == Hello" $
            do
                execTest
                    [ push Nothing (S "hELLO"),
                      push Nothing (S "hELLO"),
                      call Nothing "eq",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute nequality Hello /= Hello" $
            do
                execTest
                    [ push Nothing (S "p"),
                      push Nothing (S "hELLO"),
                      call Nothing "neq",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3 < 5" $
            do
                execTest
                    [ push Nothing (N 3),
                      push Nothing (N 5),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3.0 < 5" $
            do
                execTest
                    [ push Nothing (D 3.0),
                      push Nothing (N 5),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3 < 5.0" $
            do
                execTest
                    [ push Nothing (N 3),
                      push Nothing (D 5.0),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 4.0 < 50.0" $
            do
                execTest
                    [ push Nothing (D 4.0),
                      push Nothing (D 50.0),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 10 > 2" $
            do
                execTest
                    [ push Nothing (N 10),
                      push Nothing (N 2),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 10.0 > 2" $
            do
                execTest
                    [ push Nothing (D 10.0),
                      push Nothing (N 2),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 10 > 3.0" $
            do
                execTest
                    [ push Nothing (N 10),
                      push Nothing (D 3.0),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 25.5 > 0.2" $
            do
                execTest
                    [ push Nothing (D 25.5),
                      push Nothing (D 0.2),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute logical and true && false" $
            do
                execTest
                    [ push Nothing (B True),
                      push Nothing (B False),
                      call Nothing "and",
                      ret Nothing
                    ]
                `shouldReturn` B False

        it "should execute logical or true || false" $
            do
                execTest
                    [ push Nothing (B True),
                      push Nothing (B False),
                      call Nothing "or",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute logical not" $
            do
                execTest
                    [ push Nothing (B True),
                      call Nothing "not",
                      ret Nothing
                    ]
                `shouldReturn` B False

        notEnoughArg "and"
        notEnoughArg "greater"
        notEnoughArg "or"
        notEnoughArg "less"
        notEnoughArg "not"

        it "should execute mixed comparison 3.14 > 3" $
            do
                execTest
                    [push Nothing (D 3.14), push Nothing (N 3), call Nothing "greater", ret Nothing]
                `shouldReturn` B True
