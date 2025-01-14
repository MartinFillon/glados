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
    Instruction,
    Value (..),
    call,
    get,
    load,
    push,
    ret,
    void,
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

testIOFileOperation :: String -> [Instruction] -> String -> String -> Spec
testIOFileOperation s i file mode =
    it s $
        execTest
            ( [ push Nothing (S mode),
                push Nothing (S file),
                call Nothing "open"
              ]
                ++ i
                ++ [call Nothing "close", ret Nothing]
            )
            `shouldReturn` N 0

testIOFileOperationError :: String -> [Instruction] -> Spec
testIOFileOperationError s i =
    it s $
        execTest
            (i ++ [call Nothing "error", ret Nothing])
            `shouldReturn` B True

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
    testIOFileOperation
        "shoudl open and close a file"
        []
        "test/VirtualMachine/Operators/IOFiles/test1.txt"
        "r"

    testIOFileOperation
        "shoudl open, read and close a file"
        [call Nothing "read", void Nothing]
        "test/VirtualMachine/Operators/IOFiles/test2.txt"
        "r"

    testIOFileOperation
        "shoudl open, write a text and close a file"
        [ push Nothing $
            S "Vous savez, moi, je ne crois pas qu'il y ait de bonne ou de mauvaise situation. Moi, si je devais résumer ma vie aujourd'hui avec vous, je dirais que c'est d'abord des rencontres. Des gens qui m'ont tendu la main, peut-être à un moment où je ne pouvais pas, où j'étais seul chez-moi. Et c'est assez curieux de se dire que les hasards, les rencontres forgent une destinée… Parce que quand on a le goût de la chose, quand on a le goût de la chose bien faite, le beau geste, parfois, on ne trouve pas l'interlocuteur en face, je dirais, le miroir qui vous aide à avancer. Alors ça n'est pas mon cas, comme je disais là, puisque moi au contraire, j'ai pu : et je dis merci à la vie, je lui dis merci, je chante la vie, je danse la vie… Je ne suis qu'amour ! Et finalement, quand beaucoup de gens, aujourd'hui, me disent « Mais comment fais-tu pour avoir cette humanité ? » , et bien je leur réponds très simplement, je leur dis que c'est ce goût de l'amour ce goût donc qui m'a poussé aujourd'hui à entreprendre une construction mécanique, mais demain qui sait ? Peut-être simplement à me mettre au service de la communauté, à faire le don, le don de soi…",
          call Nothing "write",
          call Nothing "print",
          void Nothing
        ]
        "test/VirtualMachine/Operators/IOFiles/test3.txt"
        "w"

    testIOFileOperation
        "shoudl open, write a char and close a file"
        [ push Nothing $ C 'c',
          call Nothing "write",
          call Nothing "print",
          void Nothing
        ]
        "test/VirtualMachine/Operators/IOFiles/test3.txt"
        "w"

    testIOFileOperation
        "shoudl open, write a Int and close a file"
        [ push Nothing $ N 42,
          call Nothing "write",
          call Nothing "print",
          void Nothing
        ]
        "test/VirtualMachine/Operators/IOFiles/test3.txt"
        "w"

    testIOFileOperation
        "shoudl open, append a Int and close a file"
        [ push Nothing $ N 84,
          call Nothing "write",
          call Nothing "print",
          void Nothing
        ]
        "test/VirtualMachine/Operators/IOFiles/test3.txt"
        "a"

    testIOFileOperation
        "shoudl open in read write mode, write a Int and close a file"
        [ push Nothing $ N 84,
          call Nothing "write",
          call Nothing "print",
          void Nothing
        ]
        "test/VirtualMachine/Operators/IOFiles/test4.txt"
        "rw"

    testIOFileOperation
        "shoudl open in read mode, getLine, print the line and close a file"
        [ call Nothing "getLine",
          call Nothing "print",
          void Nothing
        ]
        "test/VirtualMachine/Operators/IOFiles/test4.txt"
        "r"

    testIOFileOperationError
        "shoudl fail on unwkown file"
        [ push Nothing (S "r"),
          push Nothing (S "sdkoqsdoqskjdsdpoqskdqpsodkqspdk"),
          call Nothing "open",
          call Nothing "print"
        ]

    testIOFileOperationError
        "shoudl fail on unwkown mode"
        [ push Nothing (S "adqsdqsd"),
          push Nothing (S "test/VirtualMachine/Operators/IOFiles/test3.txt"),
          call Nothing "open",
          call Nothing "print"
        ]
