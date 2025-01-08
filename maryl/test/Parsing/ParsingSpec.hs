{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ParsingSpec
-}

module Parsing.ParsingSpec (spec) where

import Data.Either (isLeft)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), ParserError, Variable (..), parseAST)
import Test.Hspec (Spec, context, describe, it, shouldBe)

parseAST' :: String -> Either ParserError [Ast]
parseAST' = parseAST

spec :: Spec
spec = do
  describe "Parsing Maryl language constructs" $ do
    context "Variable declaration" $ do
      it "int foo = 42;" $ do
        parseAST' "int foo = 42;"
          `shouldBe` Right [AstDefineVar (Variable "foo" Integer (AstInt 42))]

      it "string name = \"John\";" $ do
        parseAST' "string name = \"John\";"
          `shouldBe` Right [AstDefineVar (Variable "name" String (AstString "John"))]

    context "Function declaration" $ do
      it "int add(int a, int b) { return a + b; }" $ do
        parseAST' "int add(int a, int b) { return a + b; }"
          `shouldBe` Right [AstDefineFunc (Function "add" [AstDefineVar (Variable "a" Integer AstVoid), AstDefineVar (Variable "b" Integer AstVoid)] [AstReturn (AstBinaryFunc "+" (AstVar "a") (AstVar "b"))] Integer)]

    context "If statement" $ do
      it "if (a < b) { return a; } else { return b; }" $ do
        parseAST' "if (a < b) { return a; } else { return b; }"
          `shouldBe` Right [AstIf (AstBinaryFunc "<" (AstVar "a") (AstVar "b")) (AstBlock [AstReturn (AstVar "a")]) [] (Just (AstBlock [AstReturn (AstVar "b")]))]

    context "While loop" $ do
      it "while (i < 10) { i = i + 1; }" $ do
        parseAST' "while (i < 10) { i = i + 1; }"
          `shouldBe` Right [AstLoop (AstBinaryFunc "<" (AstVar "i") (AstInt 10)) (AstBlock [AstBinaryFunc "=" (AstVar "i") (AstBinaryFunc "+" (AstVar "i") (AstInt 1))])]

    context "List parsing" $ do
      it "[1, 2, 3]" $ do
        parseAST' "[1, 2, 3]"
          `shouldBe` Right [AstList [AstInt 1, AstInt 2, AstInt 3]]

    context "Operator parsing" $ do
      it "1 + 2 * 3" $ do
        parseAST' "1 + 2 * 3;"
          `shouldBe` Right [AstBinaryFunc "+" (AstInt 1) (AstBinaryFunc "*" (AstInt 2) (AstInt 3))]

    context "Invalid syntax" $ do
      it "int = 42;" $ do
        isLeft (parseAST' "int = 42;") `shouldBe` True
