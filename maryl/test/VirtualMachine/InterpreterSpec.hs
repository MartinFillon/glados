{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- InterpreterSpec
-}

module VirtualMachine.InterpreterSpec (spec) where

import qualified Data.Map as Map
import Test.Hspec (Spec, anyException, describe, it, shouldReturn, shouldThrow)
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
import VirtualMachine.Operators (operators)
import VirtualMachine.State (V (..))

import qualified VirtualMachine.Operators.StructsSpec as StructsSpec
import qualified VirtualMachine.Operators.IOSpec as IOSpec
import qualified VirtualMachine.Operators.LogicSpec as LogicSpec
import qualified VirtualMachine.Operators.MathematicalSpec as MathematicalSpec
import qualified VirtualMachine.Operators.StringSpec as StringSpec
import VirtualMachine.TestUtils (execTest, execTest')

factCode :: [Instruction]
factCode =
    [ pushArg Nothing 0,
      push Nothing (N 0),
      call Nothing "eq",
      jumpf Nothing (Left 2),
      push Nothing (N 1),
      ret Nothing,
      pushArg Nothing 0,
      push Nothing (N 1),
      call Nothing "sub",
      call Nothing "fact",
      pushArg Nothing 0,
      call Nothing "mul",
      ret Nothing
    ]

factCode' :: [Instruction]
factCode' =
    [ pushArg (Just ".fact") 0,
      push Nothing (N 0),
      call Nothing "eq",
      jumpf Nothing (Left 2),
      push Nothing (N 1),
      ret Nothing,
      pushArg Nothing 0,
      push Nothing (N 1),
      call Nothing "sub",
      call Nothing ".fact",
      pushArg Nothing 0,
      call Nothing "mul",
      ret Nothing
    ]

spec :: Spec
spec = do
    describe "VirtualMachine Interpreter Spec" $ do
        LogicSpec.spec
        MathematicalSpec.spec
        IOSpec.spec
        StringSpec.spec
        StructsSpec.spec

        it "should execute factorial" $ do
            let mem = Map.insert "fact" (V $ Bi factCode) (Map.fromList operators)
                code = [push Nothing (N 5), call Nothing "fact", ret Nothing]
            execTest' code mem `shouldReturn` N 120

        it "should execute factorial from label" $ do
            let code = [push Nothing (N 5), call Nothing ".fact", ret Nothing] ++ factCode'
            execTest code `shouldReturn` N 120

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

        describe "String Operations" $ do
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
                    `shouldThrow` anyException

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
                    `shouldThrow` anyException

            it "should extract substring" $ do
                execTest
                    [ push Nothing (S "Hello World"),
                      push Nothing (N 6),
                      push Nothing (N 5),
                      call Nothing "substr",
                      ret Nothing
                    ]
                    `shouldReturn` S "World"

            it "should fail substr with invalid arguments" $ do
                execTest
                    [ push Nothing (N 42),
                      push Nothing (N 0),
                      push Nothing (N 5),
                      call Nothing "substr",
                      ret Nothing
                    ]
                    `shouldThrow` anyException

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
