{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- SerializeSpec
-}

module Compiler.SerializeSpec (spec) where

import Compiler.WriteASM (serializeFunction, serializeInstArgs, serializeInstruction, serializeInstructions, serializeMemoryFunctions)
import qualified Data.Map as Map
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), Variable (..))
import System.IO ()
import Test.Hspec (Spec, describe, it, shouldBe)
import VirtualMachine.Instructions (Inst (..), Value (..), call)

definedFuncsTest :: String
definedFuncsTest =
    ".add2 pushArg 0\npushArg 1\ncall \"add\"\nret\n\n\
    \.mysucc pushArg 0\npush 1\ncall \"add\"\nret\n\n"

spec :: Spec
spec = do
    describe "serializeInstArgs" $ do
        it "serializes Push 42" $ do
            serializeInstArgs (Push (N 42)) `shouldBe` " 42"

        it "serializes Push True" $ do
            serializeInstArgs (Push (B True)) `shouldBe` " True"

        it "serializes Push \"hello\"" $ do
            serializeInstArgs (Push (S "hello")) `shouldBe` " \"hello\""

        it "serializes Push 0.2" $ do
            serializeInstArgs (Push (D 0.2)) `shouldBe` " 0.2"

        it "serializes Jump (Left 10)" $ do
            serializeInstArgs (Jump (Left 10)) `shouldBe` " 10"

        it "serializes Jump (Right \"label\")" $ do
            serializeInstArgs (Jump (Right "label")) `shouldBe` " .label"

        it "serializes Jumpf (Left 10)" $ do
            serializeInstArgs (JumpIfFalse (Left 10)) `shouldBe` " 10"

        it "serializes Jumpf (Right \"label\")" $ do
            serializeInstArgs (JumpIfFalse (Right "label")) `shouldBe` " .label"

    describe "serializeInstruction" $ do
        -- it "serializes instruction with label" $ do

        it "serializes instruction without label" $ do
            let instr = call Nothing "add"
            serializeInstruction instr `shouldBe` "call \"add\""

    --   describe "serializeInstructions" $ do
    --     it "serializes a list of instructions" $ do

    --   describe "serializeFunction" $ do
    --     it "serializes a function with instructions" $ do

    describe "serializeMemoryFunctions" $ do
        it "serializes memory with a single function" $ do
            let memory = Map.singleton "foo" (AstDefineFunc (Function "foo" [] [AstReturn (AstInt 0)] Void))
                expected = ".foo push 0\nret\n\n"
            serializeMemoryFunctions memory `shouldBe` expected

        it "serializes memory with multiple functions" $ do
            let memory =
                    Map.fromList
                        [ ("foo", AstDefineVar (Variable "foo" Integer (AstInt 4))),
                            ( "add2",
                              AstDefineFunc
                                ( Function
                                    "add"
                                    [AstVar "x", AstVar "y"]
                                    [AstReturn (AstBinaryFunc "+" (AstVar "x") (AstVar "y"))]
                                    Integer
                                )
                            ),
                            ( "mysucc",
                              AstDefineFunc
                                ( Function
                                    "add"
                                    [AstVar "x"]
                                    [AstReturn (AstBinaryFunc "+" (AstVar "x") (AstInt 1))]
                                    Integer
                                )
                            )
                        ]
                expected = definedFuncsTest
                actual = serializeMemoryFunctions memory
            actual `shouldBe` expected
