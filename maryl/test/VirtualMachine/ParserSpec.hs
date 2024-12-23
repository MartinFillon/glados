{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- ParserSpec
-}

module VirtualMachine.ParserSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe)
import VirtualMachine.Instructions (Val (..), call, jump, jumpf, push, pushArg, ret)
import VirtualMachine.Parser (parseAssembly)

spec :: Spec
spec = do
    describe "VirtualMachine Parser Spec" $ do
        it "should parse push with arg Int" $ do
            parseAssembly "push 1" `shouldBe` Right [push Nothing $ N 1]
        it "should parse push with arg Bool true" $ do
            parseAssembly "push true" `shouldBe` Right [push Nothing $ B True]
        it "should parse push with arg Bool false" $ do
            parseAssembly "push false" `shouldBe` Right [push Nothing $ B False]
        it "should parse ret" $ do
            parseAssembly "ret" `shouldBe` Right [ret Nothing]
        it "should parse jumpf with arg int" $ do
            parseAssembly "jumpf 64" `shouldBe` Right [jumpf Nothing (Left 64)]
        it "should parse jumpf with arg label" $ do
            parseAssembly "jumpf .test" `shouldBe` Right [jumpf Nothing (Right ".test")]
        it "should parse jump with arg int" $ do
            parseAssembly "jump 64" `shouldBe` Right [jump Nothing (Left 64)]
        it "should parse jump with arg label" $ do
            parseAssembly "jump .test" `shouldBe` Right [jump Nothing (Right ".test")]
        it "should parse call" $ do
            parseAssembly "call" `shouldBe` Right [call Nothing]
        it "should parse pushArg with arg" $ do
            parseAssembly "pushArg 64" `shouldBe` Right [pushArg Nothing 64]
        it "should parse push with char" $ do
            parseAssembly "push a" `shouldBe` Right [push Nothing $ C 'a']
        it "should parse push with string" $ do
            parseAssembly "push \"42\"" `shouldBe` Right [push Nothing $ S "42"]
        it "should parse push with arg Int and label" $ do
            parseAssembly ".test push 1" `shouldBe` Right [push (Just ".test") $ N 1]
        it "should fail for bad int" $ do
            isLeft (parseAssembly "push - 1") `shouldBe` True
        it "should parse a push float" $ do
            parseAssembly "push 42.42" `shouldBe` Right [push Nothing $ D 42.42]
        it "should parse a push list" $ do
            parseAssembly "push [1, 2, 3]" `shouldBe` Right [push Nothing $ L [N 1, N 2, N 3]]
