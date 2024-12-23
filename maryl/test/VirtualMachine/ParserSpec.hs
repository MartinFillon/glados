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
        it "should parse a push list of int" $ do
            parseAssembly "push [1, 2, 3]" `shouldBe` Right [push Nothing $ L [N 1, N 2, N 3]]
        it "should parse a push list of list of int" $ do
            parseAssembly "push [[1, 2], [1]]" `shouldBe` Right [push Nothing $ L [L [N 1, N 2], L [N 1]]]
        it "should parse a push list of char" $ do
            parseAssembly "push [a, b, c]" `shouldBe` Right [push Nothing $ L [C 'a', C 'b', C 'c']]
        it "should parse a push list of string" $ do
            parseAssembly "push [\"1\", \"2\", \"3\"]" `shouldBe` Right [push Nothing $ L [S "1", S "2", S "3"]]
        it "should parse a push list of floats" $ do
            parseAssembly "push [1.0, 2.0, 3.0]" `shouldBe` Right [push Nothing $ L [D 1.0, D 2.0, D 3.0]]
        it "should parse a push list with one elem" $ do
            parseAssembly "push [1]" `shouldBe` Right [push Nothing $ L [N 1]]
        it "should parse a push list with no elem" $ do
            parseAssembly "push []" `shouldBe` Right [push Nothing $ L []]
        it "should parse a push list bool" $ do
            parseAssembly "push [true, false]" `shouldBe` Right [push Nothing $ L [B True, B False]]
        it "should fail on list of bool and string" $ do
            isLeft (parseAssembly "push [true, \"false\"]") `shouldBe` True
        it "should fail on list of int and string" $ do
            isLeft (parseAssembly "push [1, \"false\"]") `shouldBe` True
        it "should not fail on list of list and string" $ do
            isLeft (parseAssembly "push [[], \"false\"]") `shouldBe` False
        it "should fail on list of char and string" $ do
            isLeft (parseAssembly "push [a, \"false\"]") `shouldBe` True
        it "should fail on list of float and string" $ do
            isLeft (parseAssembly "push [42.42, \"false\"]") `shouldBe` True
        it "should fail on list of list of int and string" $ do
            isLeft (parseAssembly "push [[1], \"false\"]") `shouldBe` True
        it "should fail on string and int" $ do
            isLeft (parseAssembly "push [\"false\", 1]") `shouldBe` True
