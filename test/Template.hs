{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Template
-}

module Template (spec) where

import Test.Hspec

add :: Int -> Int -> Int
add x y = x + y

spec :: Spec
spec = do
    describe "add function" $ do
        context "addition of one number and another" $ do
            it "adds two positive numbers correctly" $ do
                add 2 3 `shouldBe` 5
