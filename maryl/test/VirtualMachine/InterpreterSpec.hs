{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- InterpreterSpec
-}

module VirtualMachine.InterpreterSpec (spec) where

import Control.Exception (IOException)
import Control.Monad.State (evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Error (isDoesNotExistError)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import VirtualMachine.Instructions (
    Instruction,
    Value (..),
    call,
    get,
    jump,
    jumpf,
    load,
    push,
    pushArg,
    ret,
 )
import VirtualMachine.Interpreter (exec)
import VirtualMachine.Operators (operators)
import VirtualMachine.State (V (..), initialState)

import qualified VirtualMachine.Operators.LogicSpec as LogicSpec
import qualified VirtualMachine.Operators.MathematicalSpec as MathematicalSpec
import VirtualMachine.TestUtils (constIO, execTest, execTest')

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
        it "should execute factorial" $ do
            let mem = Map.insert "fact" (V $ Bi factCode) (Map.fromList operators)
                code = [push Nothing (N 5), call Nothing "fact", ret Nothing]
            execTest' code mem `shouldReturn` N 120

        it "should execute factorial from label" $ do
            let code = [push Nothing (N 5), call Nothing ".fact", ret Nothing] ++ factCode'
            execTest code `shouldReturn` N 120

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

        it "should set element in memory and retrieve it" $
            do
                execTest
                    [ load Nothing "v" $ N 42,
                      get Nothing "v",
                      ret Nothing
                    ]
                `shouldReturn` N 42

        it "should write content to a file and read it back" $ do
            execTest
                [ push Nothing (S "test.txt"),
                  push Nothing (S "Hello, World!"),
                  call Nothing "writeFile",
                  push Nothing (S "test.txt"),
                  call Nothing "readFile",
                  ret Nothing
                ]
                `shouldReturn` S "Hello, World!"

        it "should return length of written content" $ do
            execTest
                [ push Nothing (S "test.txt"),
                  push Nothing (S "Hello Man"),
                  call Nothing "writeFile",
                  ret Nothing
                ]
                `shouldReturn` N 9

        it "should append content to a file" $ do
            execTest
                [ push Nothing (S "test.txt"),
                  push Nothing (S "First line\n"),
                  call Nothing "writeFile",
                  push Nothing (S "test.txt"),
                  push Nothing (S "Second line\n"),
                  call Nothing "appendFile",
                  push Nothing (S "test.txt"),
                  call Nothing "readFile",
                  ret Nothing
                ]
                `shouldReturn` S "First line\nSecond line\n"

        it "should handle reading non-existent file" $ do
            execTest
                [ push Nothing (S "nonexistent.txt"),
                  call Nothing "readFile",
                  ret Nothing
                ]
                `shouldThrow` \e -> isDoesNotExistError (e :: IOException)
