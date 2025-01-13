{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- StringSpec
-}

module VirtualMachine.Operators.StringSpec (spec) where

import Control.Exception (IOException)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import VirtualMachine.Instructions (
    Value (..),
    call,
    push,
    ret,
 )
import VirtualMachine.TestUtils (execTest, userError')

spec :: Spec
spec = do
    describe "String Operators" $ do
        it "should concatenate two strings" $ do
            execTest
                [ push Nothing (S "Hello "),
                  push Nothing (S "World"),
                  call Nothing "strcat",
                  ret Nothing
                ]
                `shouldReturn` S "Hello World"

        it "should fail strcat with non-string arguments" $ do
            execTest
                [ push Nothing (N 42),
                  push Nothing (S "World"),
                  call Nothing "strcat",
                  ret Nothing
                ]
                `shouldThrow` userError' "strcat expects two strings"

        it "should get string length" $ do
            execTest
                [ push Nothing (S "Hello"),
                  call Nothing "strlen",
                  ret Nothing
                ]
                `shouldReturn` N 5

        it "should fail strlen with non-string argument" $ do
            execTest
                [ push Nothing (N 42),
                  call Nothing "strlen",
                  ret Nothing
                ]
                `shouldThrow` userError' "strlen expects a string"

        it "should extract substring" $ do
            execTest
                [ push Nothing (S "Hello World"),
                  push Nothing (N 6),
                  push Nothing (N 5),
                  call Nothing "substr",
                  ret Nothing
                ]
                `shouldReturn` S "World"

        it "should fail substr with invalid range" $ do
            execTest
                [ push Nothing (S "Hello"),
                  push Nothing (N 10),
                  push Nothing (N 5),
                  call Nothing "substr",
                  ret Nothing
                ]
                `shouldThrow` userError' "substr: invalid range"

        it "should fail substr with non-string argument" $ do
            execTest
                [ push Nothing (N 42),
                  push Nothing (N 0),
                  push Nothing (N 5),
                  call Nothing "substr",
                  ret Nothing
                ]
                `shouldThrow` userError' "substr expects a string and two numbers"

        it "should compare equal strings" $ do
            execTest
                [ push Nothing (S "Hello"),
                  push Nothing (S "Hello"),
                  call Nothing "strcmp",
                  ret Nothing
                ]
                `shouldReturn` N 0

        it "should compare different strings" $ do
            execTest
                [ push Nothing (S "Hello"),
                  push Nothing (S "World"),
                  call Nothing "strcmp",
                  ret Nothing
                ]
                `shouldReturn` N (-1)

