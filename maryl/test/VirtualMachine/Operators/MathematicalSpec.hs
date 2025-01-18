{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- MathematicalSpec
-}

module VirtualMachine.Operators.MathematicalSpec (spec) where

import Control.Exception (IOException)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import VirtualMachine.Instructions (
    Value (..),
    call,
    push,
    ret,
 )
import VirtualMachine.TestUtils (execTest, userError')

notEnoughArgM :: String -> (IOException -> Bool) -> Spec
notEnoughArgM s f =
    it ("should fail cause not enough arg on " ++ s) $
        execTest [push Nothing (N 10), call Nothing s] `shouldThrow` f

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

        it "should fail on division of by 0" $
            do
                execTest
                    [push Nothing (N 10), push Nothing (N 0), call Nothing "div", ret Nothing]
                `shouldThrow` userError' "division by zero"

        it "should fail on division of by 0.0" $
            do
                execTest
                    [push Nothing (D 10.0), push Nothing (D 0.0), call Nothing "div", ret Nothing]
                `shouldThrow` userError' "division by zero"

        it "should do 10.0 / 5.0" $
            do
                execTest
                    [push Nothing (D 10.0), push Nothing (D 5.0), call Nothing "div", ret Nothing]
                `shouldReturn` D 2

        it "should fail on modulo by 0" $
            do
                execTest
                    [push Nothing (N 10), push Nothing (N 0), call Nothing "mod", ret Nothing]
                `shouldThrow` userError' "modulo by zero"

        notEnoughArgM "add" $ userError' "expects two number"
        notEnoughArgM "sub" $ userError' "expects two number"
        notEnoughArgM "mul" $ userError' "expects two number"
        notEnoughArgM "div" $ userError' "expects two number"
        notEnoughArgM "mod" $ userError' "expects two int"
