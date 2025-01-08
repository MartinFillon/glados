{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- ParserSpec
-}

module VirtualMachine.ParserSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe)
import VirtualMachine.Instructions (
    Value (..),
    call,
    get,
    jump,
    jumpf,
    load,
    push,
    pushArg,
    ret,
    vmAppendFile,
    vmReadFile,
    vmWriteFile,
 )
import VirtualMachine.Parser (parseAssembly)

spec :: Spec
spec = do
    describe "VirtualMachine Parser Spec" $ do
        it "should parse push with arg Int" $ do
            parseAssembly "push 1" `shouldBe` Right [Left $ push Nothing $ N 1]
        it "should parse push with arg Bool true" $ do
            parseAssembly "push true" `shouldBe` Right [Left $ push Nothing $ B True]
        it "should parse push with arg Bool false" $ do
            parseAssembly "push false" `shouldBe` Right [Left $ push Nothing $ B False]
        it "should parse ret" $ do
            parseAssembly "ret" `shouldBe` Right [Left $ ret Nothing]
        it "should parse jumpf with arg int" $ do
            parseAssembly "jumpf 64" `shouldBe` Right [Left $ jumpf Nothing (Left 64)]
        it "should parse jumpf with arg label" $ do
            parseAssembly "jumpf .test"
                `shouldBe` Right [Left $ jumpf Nothing (Right ".test")]
        it "should parse jump with arg int" $ do
            parseAssembly "jump 64" `shouldBe` Right [Left $ jump Nothing (Left 64)]
        it "should parse jump with arg label" $ do
            parseAssembly "jump .test"
                `shouldBe` Right [Left $ jump Nothing (Right ".test")]
        it "should parse pushArg with arg" $ do
            parseAssembly "pushArg 64" `shouldBe` Right [Left $ pushArg Nothing 64]
        -- it "should parse push with char" $ do
        -- parseAssembly "push a" `shouldBe` Right [Left $ push Nothing $ C 'a']
        it "should parse push with string" $ do
            parseAssembly "push \"42\"" `shouldBe` Right [Left $ push Nothing $ S "42"]
        it "should parse push with arg Int and label" $ do
            parseAssembly ".test push 1" `shouldBe` Right [Left $ push (Just ".test") $ N 1]
        it "should fail for bad int" $ do
            isLeft (parseAssembly "push - 1") `shouldBe` True
        it "should parse a push float" $ do
            parseAssembly "push 42.42" `shouldBe` Right [Left $ push Nothing $ D 42.42]
        it "should parse a push list of int" $ do
            parseAssembly "push [1, 2, 3]"
                `shouldBe` Right [Left $ push Nothing $ L [N 1, N 2, N 3]]
        it "should parse a push list of list of int" $ do
            parseAssembly "push [[1, 2], [1]]"
                `shouldBe` Right [Left $ push Nothing $ L [L [N 1, N 2], L [N 1]]]
        it "should parse a push list of string" $ do
            parseAssembly "push [\"1\", \"2\", \"3\"]"
                `shouldBe` Right [Left $ push Nothing $ L [S "1", S "2", S "3"]]
        it "should parse a push list of floats" $ do
            parseAssembly "push [1.0, 2.0, 3.0]"
                `shouldBe` Right [Left $ push Nothing $ L [D 1.0, D 2.0, D 3.0]]
        it "should parse a push list with one elem" $ do
            parseAssembly "push [1]" `shouldBe` Right [Left $ push Nothing $ L [N 1]]
        it "should parse a push list with no elem" $ do
            parseAssembly "push []" `shouldBe` Right [Left $ push Nothing $ L []]
        it "should parse a push list bool" $ do
            parseAssembly "push [true, false]"
                `shouldBe` Right [Left $ push Nothing $ L [B True, B False]]
        it "shoudl parse a get" $ do
            parseAssembly "get \"hello\"" `shouldBe` Right [Left $ get Nothing "hello"]
        it "shoudl parse a load" $ do
            parseAssembly "load \"hello\" 1"
                `shouldBe` Right [Left $ load Nothing "hello" $ N 1]
        it "shoudl parse a load with a label" $ do
            parseAssembly ".test load \"hello\" 1"
                `shouldBe` Right [Left $ load (Just ".test") "hello" $ N 1]
        it "shoudl parse a get with a label" $ do
            parseAssembly ".test get \"hello\""
                `shouldBe` Right [Left $ get (Just ".test") "hello"]
        it "should fail on a load with int int" $ do
            isLeft (parseAssembly "load 1 1") `shouldBe` True
        it "should fail on a get with int" $ do
            isLeft (parseAssembly "get 1") `shouldBe` True
        it "should parse a call to a string" $ do
            parseAssembly ".test call \"42\""
                `shouldBe` Right [Left $ call (Just ".test") "42"]
        it "should parse a call to a string without label" $ do
            parseAssembly "call \"42\"" `shouldBe` Right [Left $ call Nothing "42"]
        it "should fail on a call with int" $ do
            isLeft (parseAssembly "call 1") `shouldBe` True
        it "should parse a simple add1 function" $ do
            parseAssembly
                ".header_function      \"add1\" push 1 pushArg 0 call \"add\" ret .footer_function"
                `shouldBe` Right
                    [ Right
                        ( "add1",
                          [push Nothing $ N 1, pushArg Nothing 0, call Nothing "add", ret Nothing]
                        )
                    ]

        it "should parse readFile instruction" $ do
            parseAssembly "readFile \"test.txt\""
                `shouldBe` Right [vmReadFile Nothing "test.txt"]

        it "should parse readFile with label" $ do
            parseAssembly ".read readFile \"test.txt\""
                `shouldBe` Right [vmReadFile (Just ".read") "test.txt"]

        it "should parse writeFile instruction" $ do
            parseAssembly "writeFile \"test.txt\" \"Hello\""
                `shouldBe` Right [vmWriteFile Nothing "test.txt" "Hello"]

        it "should parse writeFile with label" $ do
            parseAssembly ".write writeFile \"test.txt\" \"Hello\""
                `shouldBe` Right [vmWriteFile (Just ".write") "test.txt" "Hello"]

        it "should parse appendFile instruction" $ do
            parseAssembly "appendFile \"test.txt\" \"World\""
                `shouldBe` Right [vmAppendFile Nothing "test.txt" "World"]

        it "should parse appendFile with label" $ do
            parseAssembly ".append appendFile \"test.txt\" \"World\""
                `shouldBe` Right [vmAppendFile (Just ".append") "test.txt" "World"]

        it "should fail on readFile with non-string argument" $ do
            isLeft (parseAssembly "readFile 42") `shouldBe` True

        it "should fail on writeFile with non-string arguments" $ do
            isLeft (parseAssembly "writeFile 42 \"Hello\"") `shouldBe` True
            isLeft (parseAssembly "writeFile \"test.txt\" 42") `shouldBe` True

        it "should fail on appendFile with non-string arguments" $ do
            isLeft (parseAssembly "appendFile 42 \"World\"") `shouldBe` True
            isLeft (parseAssembly "appendFile \"test.txt\" 42") `shouldBe` True
