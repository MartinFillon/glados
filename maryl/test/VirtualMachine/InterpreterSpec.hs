{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- InterpreterSpec
-}

module VirtualMachine.InterpreterSpec (spec) where

import Control.Monad.State (evalStateT)
import Test.Hspec (Spec, describe, it, shouldReturn)
import VirtualMachine.Instructions (
    Instruction,
    Value (..),
    call,
    jump,
    jumpf,
    push,
    pushArg,
    ret,
 )
import VirtualMachine.Interpreter (exec)
import VirtualMachine.State (initialState)

-- factCode :: [Instruction]
-- factCode =
-- [ pushArg Nothing 0,
--   push Nothing (N 0),
--   call Nothing "eq",
--   jumpf Nothing (Left 2),
--   push Nothing (N 1),
--   ret Nothing,
--   pushArg Nothing 0,
--   push Nothing (N 1),
--   call Nothing "sub",
--   call Nothing "fact",
--   pushArg Nothing 0,
--   call Nothing "mul",
--   ret Nothing
-- ]

execTest :: [Instruction] -> IO Value
execTest is = evalStateT exec (initialState is [])

spec :: Spec
spec = do
    describe "VirtualMachine Interpreter Spec" $ do
        -- it "should execute factorial" $ do
        --     let mem = Map.insert "fact" (Bi factCode) initialMemory
        --         args = []
        --         code = [push Nothing (N 5), call Nothing "fact", Ret]
        --     result <- exec mem args code []
        --     result `shouldBe` Right (N 120)

        it "should execute addition 10 + 30 + 20" $ do
            execTest
                [ push Nothing (N 10),
                  push Nothing (N 30),
                  call Nothing "add",
                  push Nothing (N 20),
                  call Nothing "add",
                  ret Nothing
                ]
                `shouldReturn` N 60

        it "should execute mixed addition (5 + 3.14)" $ do
            execTest
                [ push Nothing (N 5),
                  push Nothing (D 3.14),
                  call Nothing "add",
                  ret Nothing
                ]
                `shouldReturn` D 8.14

        it "should execute mixed multiplication (2.5 * 3)" $
            do
                execTest
                    [ push Nothing (D 2.5),
                      push Nothing (N 3),
                      call Nothing "mul",
                      ret Nothing
                    ]
                `shouldReturn` D 7.5

        it "should execute subtraction 30 - 10.5" $
            do
                execTest
                    [push Nothing (D 30), push Nothing (D 10.5), call Nothing "sub", ret Nothing]
                    `shouldReturn` D 19.5

        it "should execute division 40 / 8" $
            do
                execTest
                    [push Nothing (N 40), push Nothing (N 8), call Nothing "div", ret Nothing]
                `shouldReturn` N 5

        it "should execute mixed division (5.0 / 2)" $
            do
                execTest
                    [push Nothing (D 5.0), push Nothing (N 2), call Nothing "div", ret Nothing]
                `shouldReturn` D 2.5

        it "should execute modulo 10 % 3" $
            do
                execTest
                    [push Nothing (N 10), push Nothing (N 3), call Nothing "mod", ret Nothing]
                `shouldReturn` N 1

        it "should execute mixed comparison 3.14 > 3" $
            do
                execTest
                    [push Nothing (D 3.14), push Nothing (N 3), call Nothing "greater", ret Nothing]
                `shouldReturn` B True

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

        it "should execute equality Hello == Hello" $
            do
                execTest
                    [ push Nothing (S "hELLO"),
                      push Nothing (S "hELLO"),
                      call Nothing "eq",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3 < 5" $
            do
                execTest
                    [push Nothing (N 3), push Nothing (N 5), call Nothing "less", ret Nothing]
                `shouldReturn` B True

        it "should execute greater than 10 > 2" $
            do
                execTest
                    [push Nothing (N 10), push Nothing (N 2), call Nothing "greater", ret Nothing]
                `shouldReturn` B True

        it "should execute logical and true && false" $
            do
                execTest
                    [push Nothing (B True), push Nothing (B False), call Nothing "and", ret Nothing]
                `shouldReturn` B False

        it "should execute logical or true || false" $
            do
                execTest
                    [push Nothing (B True), push Nothing (B False), call Nothing "or", ret Nothing]
                `shouldReturn` B True

        it "should execute print length of 'Hello'" $
            do
                execTest [push Nothing (S "Hello"), call Nothing "print", ret Nothing]
                `shouldReturn` N 5

        it "should get element at index 1" $
            do
                execTest
                    [ push Nothing (L [N 1, N 2, N 3]),
                      push Nothing (N 1),
                      call Nothing "get",
                      ret Nothing
                    ]
                `shouldReturn` N 2

        it "should set element at index 1" $
            do
                execTest
                    [ push Nothing (L [N 1, N 2, N 3]),
                      push Nothing (N 1),
                      push Nothing (N 8),
                      call Nothing "set",
                      ret Nothing
                    ]
                `shouldReturn` L [N 1, N 8, N 3]

        it "should execute jump test" $
            do
                execTest
                    [ push Nothing (N 10),
                      jump Nothing (Left 1),
                      push Nothing (N 2),
                      ret Nothing,
                      push Nothing (N 3),
                      call Nothing "add",
                      ret Nothing
                    ]
                `shouldReturn` N 10
