{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- ParserSpec
-}

module VirtualMachine.ParserSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import VirtualMachine.Instructions (
    Val (..),
    call,
    jumpf,
    push,
    pushArg,
    ret,
 )
import VirtualMachine.Parser (parseAssembly)

spec :: Spec
spec = do
    describe "VirtualMachine Parser Spec" $ do
        it "should parse push with arg Int" $ do
            parseAssembly "push 1" `shouldBe` Right [push $ N 1]
        it "should parse push with arg Bool true" $ do
            parseAssembly "push true" `shouldBe` Right [push $ B True]
        it "should parse push with arg Bool false" $ do
            parseAssembly "push true" `shouldBe` Right [push $ B True]
        it "should parse ret" $ do
            parseAssembly "ret" `shouldBe` Right [ret]
        it "should parse jumpf with arg" $ do
            parseAssembly "jumpf 64" `shouldBe` Right [jumpf 64]
        it "should parse call" $ do
            parseAssembly "call" `shouldBe` Right [call]
        it "should parse pushArg with arg" $ do
            parseAssembly "pushArg 64" `shouldBe` Right [pushArg 64]
