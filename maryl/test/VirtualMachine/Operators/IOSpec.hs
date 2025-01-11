{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- IOSpec
-}

module VirtualMachine.Operators.IOSpec (spec) where

import Control.Exception (IOException)
import System.IO.Error (isDoesNotExistError)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import VirtualMachine.Instructions (
    Value (..),
    call,
    get,
    load,
    push,
    ret,
 )

import VirtualMachine.TestUtils (constIO, execTest)

testPrint :: Value -> Spec
testPrint s@(S v) =
    it ("should execute print length of " ++ show s) $
        execTest [push Nothing s, call Nothing "print", ret Nothing]
            `shouldReturn` N (fromIntegral $ length v)
testPrint s@(C _) =
    it ("should execute print length of " ++ show s) $
        execTest [push Nothing s, call Nothing "print", ret Nothing]
            `shouldReturn` N 1
testPrint s =
    it ("should execute print length of " ++ show s) $
        execTest [push Nothing s, call Nothing "print", ret Nothing]
            `shouldReturn` N (fromIntegral $ length $ show s)

testEmptyPrint :: Spec
testEmptyPrint =
    it "should execute print length of nothing " $
        execTest [call Nothing "print", ret Nothing]
            `shouldThrow` constIO

spec :: Spec
spec = describe "testing io functions" $ do
    testPrint $ S "hello"
    testPrint $ C 'c'
    testPrint $ N 3
    it "should set element in memory and retrieve it" $
        execTest
            [ load Nothing "v" $ N 42,
              get Nothing "v",
              ret Nothing
            ]
            `shouldReturn` N 42

    it "should write content to a file and read it back" $
        execTest
            [ push Nothing (S "test.txt"),
              push Nothing (S "Hello, World!"),
              call Nothing "writeFile",
              push Nothing (S "test.txt"),
              call Nothing "readFile",
              ret Nothing
            ]
            `shouldReturn` S "Hello, World!"

    it "should return length of written content" $
        execTest
            [ push Nothing (S "test.txt"),
              push Nothing (S "Hello Man"),
              call Nothing "writeFile",
              ret Nothing
            ]
            `shouldReturn` N 9

    it "should return length of written content" $
        execTest
            [ push Nothing (S "test.txt"),
              push Nothing (S "Hello Man"),
              call Nothing "appendFile",
              ret Nothing
            ]
            `shouldReturn` N 9

    it "should append content to a file" $
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

    it "should handle reading non-existent file" $
        execTest
            [ push Nothing (S "nonexistent.txt"),
              call Nothing "readFile",
              ret Nothing
            ]
            `shouldThrow` \e -> isDoesNotExistError (e :: IOException)
    testEmptyPrint
