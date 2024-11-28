{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- PrinterSpec
-}

{-# LANGUAGE InstanceSigs #-}

module PrinterSpec (spec) where

import Printer (Color (..), Paint (..), Style (..), reset)
import Test.Hspec (Spec, context, describe, it, shouldBe)

newtype TestPaint = TestPaint Int

instance Paint TestPaint where
    paint :: Bool -> TestPaint -> String
    paint False (TestPaint i) = show i
    paint True (TestPaint i) = show Red ++ show i ++ reset

spec :: Spec
spec = do
    describe "printing with color or style" $ do
        context "red color" $ do
            it "should be the red ansii code" $ do
                let red = Red
                show red `shouldBe` "\x1b[31m"

        context "green color" $ do
            it "should be the green ansii code" $ do
                let green = Green
                show green `shouldBe` "\x1b[32m"

        context "blue color" $ do
            it "should be the blue ansii code" $ do
                let blue = Blue
                show blue `shouldBe` "\x1b[34m"

        context "yellow color" $ do
            it "should be the yellow ansii code" $ do
                let yellow = Yellow
                show yellow `shouldBe` "\x1b[33m"

        context "magenta color" $ do
            it "should be the magenta ansii code" $ do
                let magenta = Magenta
                show magenta `shouldBe` "\x1b[35m"

        context "cyan color" $ do
            it "should be the cyan ansii code" $ do
                let cyan = Cyan
                show cyan `shouldBe` "\x1b[36m"

        context "white color" $ do
            it "should be the white ansii code" $ do
                let white = White
                show white `shouldBe` "\x1b[37m"

        context "underline style" $ do
            it "should be the underline ansii code" $ do
                let underline = Underlined
                show underline `shouldBe` "\x1b[4m"

        context "bold style" $ do
            it "should be the bold ansii code" $ do
                let bold = Bold
                show bold `shouldBe` "\x1b[1m"

        context "reset function should reset it" $ do
            it "should be the reset ansii code" $ do
                reset `shouldBe` "\x1b[0m"

        context "test if painter is not colored" $ do
            it "should return the number" $ do
                let testPaint = TestPaint 42
                paint False testPaint `shouldBe` "42"

        context "test if painter is colored" $ do
            it "should return the number with red color" $ do
                let testPaint = TestPaint 42
                paint True testPaint `shouldBe` "\x1b[31m42\x1b[0m"
