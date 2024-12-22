{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- InterpreterSpec
-}

module VirtualMachine.InterpreterSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Map as Map
import VirtualMachine.Interpreter (
    Value(..),
    Inst(..),
    initialMemory,
    exec
    )

factCode :: [Inst]
factCode =
    [
        PushArg 0,
        Push (N 0),
        Call "eq",
        JumpIfFalse 2,
        Push (N 1),
        Ret,
        PushArg 0,
        Push (N 1),
        Call "sub",
        Call "fact",
        PushArg 0,
        Call "mul",
        Ret
    ]

spec :: Spec
spec = do
    describe "VirtualMachine Interpreter Spec" $ do
        it "should execute factorial" $ do
            let mem = Map.insert "fact" (Bi factCode) initialMemory
                args = []
                code = [Push (N 5), Call "fact", Ret]
            exec mem args code [] `shouldBe` Right (N 120)

        it "should execute addition 10 + 30 + 20" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 10), Push (N 30), Call "add", Push (N 20), Call "add", Ret]
            exec mem args code [] `shouldBe` Right (N 60)

        it "should execute mixed addition (5 + 3.14)" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 5), Push (D 3.14), Call "add", Ret]
            exec mem args code [] `shouldBe` Right (D 8.14)

        it "should execute mixed multiplication (2.5 * 3)" $ do
            let mem = initialMemory
                args = []
                code = [Push (D 2.5), Push (N 3), Call "mul", Ret]
            exec mem args code [] `shouldBe` Right (D 7.5)

        it "should execute subtraction 30 - 10.5" $ do
            let mem = initialMemory
                args = []
                code = [Push (F 30), Push (F 10.5), Call "sub", Ret]
            exec mem args code [] `shouldBe` Right (F 19.5)

        it "should execute division 40 / 8" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 40), Push (N 8), Call "div", Ret]
            exec mem args code [] `shouldBe` Right (N 5)

        it "should execute mixed division (5.0 / 2)" $ do
            let mem = initialMemory
                args = []
                code = [Push (D 5.0), Push (N 2), Call "div", Ret]
            exec mem args code [] `shouldBe` Right (D 2.5)

        it "should execute modulo 10 % 3" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 10), Push (N 3), Call "mod", Ret]
            exec mem args code [] `shouldBe` Right (N 1)

        it "should execute mixed comparison 3.14 > 3" $ do
            let mem = initialMemory
                args = []
                code = [Push (D 3.14), Push (N 3), Call "greater", Ret]
            exec mem args code [] `shouldBe` Right (B True)

        it "should execute addition 10 + 30 + 20 with mixed types" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 10), Push (D 30), Call "add", Push (N 20), Call "add", Ret]
            exec mem args code [] `shouldBe` Right (D 60)

        it "should execute subtraction 30 - 10" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 30), Push (N 10), Call "sub", Ret]
            exec mem args code [] `shouldBe` Right (N 20)

        it "should execute multiplication 6 * 7" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 6), Push (N 7), Call "mul", Ret]
            exec mem args code [] `shouldBe` Right (N 42)

        it "should execute division 40 / 8" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 40), Push (N 8), Call "div", Ret]
            exec mem args code [] `shouldBe` Right (N 5)

        it "should execute modulo 10 % 3" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 10), Push (N 3), Call "mod", Ret]
            exec mem args code [] `shouldBe` Right (N 1)

        it "should execute equality Hello == Hello" $ do
            let mem = initialMemory
                args = []
                code = [Push (S "hELLO"), Push (S "hELLO"), Call "eq", Ret]
            exec mem args code [] `shouldBe` Right (B True)

        it "should execute less than 3 < 5" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 3), Push (N 5), Call "less", Ret]
            exec mem args code [] `shouldBe` Right (B True)

        it "should execute greater than 10 > 2" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 10), Push (N 2), Call "greater", Ret]
            exec mem args code [] `shouldBe` Right (B True)

        it "should execute logical and true && false" $ do
            let mem = initialMemory
                args = []
                code = [Push (B True), Push (B False), Call "and", Ret]
            exec mem args code [] `shouldBe` Right (B False)

        it "should execute logical or true || false" $ do
            let mem = initialMemory
                args = []
                code = [Push (B True), Push (B False), Call "or", Ret]
            exec mem args code [] `shouldBe` Right (B True)

        it "should execute print length of 'Hello'" $ do
            let mem = initialMemory
                args = []
                code = [Push (S "Hello"), Call "print", Ret]
            exec mem args code [] `shouldBe` Right (N 5)

        it "should get element at index 1" $ do
            let mem = initialMemory
                args = []
                code = [Push (L [N 1, N 2, N 3]), Push (N 1), Call "get", Ret]
            exec mem args code [] `shouldBe` Right (N 2)

        it "should set element at index 1" $ do
            let mem = initialMemory
                args = []
                code = [Push (L [N 1, N 2, N 3]), Push (N 1), Push (N 8), Call "set", Ret]
            exec mem args code [] `shouldBe` Right (L [N 1, N 8, N 3])

        it "should execute jump test" $ do
            let mem = initialMemory
                args = []
                code = [Push (N 10), Jump 1, Push (N 2), Ret, Push (N 3), Call "add", Ret]
            exec mem args code [] `shouldBe` Right (N 10)
