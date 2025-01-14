{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- StructsSpec
-}

module VirtualMachine.Operators.StructsSpec (spec) where

import qualified Data.Map as Map
import Test.Hspec (Spec, describe, it, shouldReturn)
import VirtualMachine.Instructions (Value (..), call, push, ret)
import VirtualMachine.TestUtils (execTest)

spec :: Spec
spec = do
    describe "basic structs functions test" $ do
        it "Should get the value at key a in a struct" $ do
            execTest
                [ push Nothing (St (Map.fromList [("a", N 1)])),
                  push Nothing (S "a"),
                  call Nothing "getField",
                  ret Nothing
                ]
                `shouldReturn` N 1

        it "Should set the value at key a in a struct and get It back" $ do
            execTest
                [ push Nothing (St (Map.fromList [("a", N 1)])),
                  push Nothing (S "a"),
                  push Nothing (N 42),
                  call Nothing "setField",
                  push Nothing (S "a"),
                  call Nothing "getField",
                  ret Nothing
                ]
                `shouldReturn` N 42
