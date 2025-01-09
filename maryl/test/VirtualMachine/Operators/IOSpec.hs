{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- IOSpec
-}

module VirtualMachine.Operators.IOSpec (spec) where

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

testPrint :: Value -> Spec
testPrint s@(S v) = it ("should execute print length of " ++ (show s)) $
    do
        execTest [push Nothing s, call Nothing "print", ret Nothing]
            `shouldReturn` N (fromIntegral $ length v)
testPrint s@(C _) = it ("should execute print length of " ++ (show s)) $
    do
        execTest [push Nothing s, call Nothing "print", ret Nothing]
            `shouldReturn` N 1
testPrint s =
    it ("should execute print length of " ++ (show s)) $
        do
            execTest [push Nothing s, call Nothing "print", ret Nothing]
                `shouldReturn` N (fromIntegral $ length $ show s)

testEmptyPrint :: Spec
testEmptyPrint = it ("should execute print length of nothing ") $
    do
        execTest [call Nothing "print", ret Nothing]
            `shouldThrow` constIO

spec :: Spec
spec = describe "testing io functions" $ do
    testPrint $ S "hello"
    testPrint $ C 'c'
    testPrint $ N 3
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

    it "should return length of written content" $ do
        execTest
            [ push Nothing (S "test.txt"),
              push Nothing (S "Hello Man"),
              call Nothing "appendFile",
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
