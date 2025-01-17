{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- EvalSpec
-}

module Eval.EvalSpec (spec) where

import Data.Either (isLeft)
import Eval.Evaluator (evalAST, evalNode)
import Memory (initMemory)
import Parsing.ParserAst (Ast(..), MarylType(..), Variable(..), Function(..), Structure(..), Ast(AstLabel))
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Evaluator tests" $ do
    context "Variable and constant evaluation" $ do
      it "int foo = 42;" $ do
        let ast = [AstDefineVar (Variable "foo" Int (AstInt 42))]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right ([AstDefineVar (Variable "foo" Int (AstInt 42))], memory)

      it "string name = \"John\";" $ do
        let ast = [AstDefineVar (Variable "name" String (AstString "John"))]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right ([AstDefineVar (Variable "name" String (AstString "John"))], memory)

    context "Function declaration and calls" $ do
      it "int add(int a, int b) { return a + b; }" $ do
        let ast = [AstDefineFunc (Function "add" [AstDefineVar (Variable "a" Int AstVoid), AstDefineVar (Variable "b" Int AstVoid)] [AstReturn (AstBinaryFunc "+" (AstVar "a") (AstVar "b"))] Int)]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right (ast, memory)

      it "Calling add(5, 3)" $ do
        let ast = [
              AstDefineFunc (Function "add" [AstDefineVar (Variable "a" Int AstVoid), AstDefineVar (Variable "b" Int AstVoid)] [AstReturn (AstBinaryFunc "+" (AstVar "a") (AstVar "b"))] Int),
              AstFunc (Function "add" [] [AstInt 5, AstInt 3] Int)]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right ([
              AstDefineFunc (Function "add" [AstDefineVar (Variable "a" Int AstVoid), AstDefineVar (Variable "b" Int AstVoid)] [AstReturn (AstBinaryFunc "+" (AstVar "a") (AstVar "b"))] Int),
              AstInt 8
            ], memory)

    context "Structures and field access" $ do
      it "struct vector {int x; int y;}" $ do
        let ast = [AstDefineStruct (Structure "vector" [AstDefineVar (Variable "x" Int AstVoid), AstDefineVar (Variable "y" Int AstVoid)])]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right (ast, memory)

    context "Control flow evaluation" $ do
      it "if (x < y) { return x; } else { return y; }" $ do
        let ast = [
              AstDefineVar (Variable "x" Int (AstInt 5)),
              AstDefineVar (Variable "y" Int (AstInt 10)),
              AstIf (AstBinaryFunc "<" (AstVar "x") (AstVar "y")) (AstBlock [AstReturn (AstVar "x")]) [] (Just (AstBlock [AstReturn (AstVar "y")]))]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right ([
              AstDefineVar (Variable "x" Int (AstInt 5)),
              AstDefineVar (Variable "y" Int (AstInt 10)),
              AstReturn (AstInt 5)
            ], memory)

      it "while (i < 3) { i = i + 1; }" $ do
        let ast = [
              AstDefineVar (Variable "i" Int (AstInt 0)),
              AstLoop Nothing (AstBinaryFunc "<" (AstVar "i") (AstInt 3)) (AstBlock [AstBinaryFunc "=" (AstVar "i") (AstBinaryFunc "+" (AstVar "i") (AstInt 1))])]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right ([
              AstDefineVar (Variable "i" Int (AstInt 0)),
              AstLoop Nothing (AstBinaryFunc "<" (AstVar "i") (AstInt 3)) (AstBlock [AstBinaryFunc "=" (AstVar "i") (AstBinaryFunc "+" (AstVar "i") (AstInt 1))])
            ], memory)

      it "Breaking from a loop" $ do
        let ast = [
              AstDefineVar (Variable "i" Int (AstInt 0)),
              AstLoop Nothing (AstBinaryFunc "<" (AstVar "i") (AstInt 5)) (AstBlock [
                AstIf (AstBinaryFunc "==" (AstVar "i") (AstInt 3)) (AstBlock [AstBreak Nothing]) [] Nothing,
                AstBinaryFunc "=" (AstVar "i") (AstBinaryFunc "+" (AstVar "i") (AstInt 1))
              ])]
        let memory = initMemory
        evalAST memory ast `shouldBe` Right ([
              AstDefineVar (Variable "i" Int (AstInt 0)),
              AstLoop Nothing (AstBinaryFunc "<" (AstVar "i") (AstInt 5)) (AstBlock [
                AstIf (AstBinaryFunc "==" (AstVar "i") (AstInt 3)) (AstBlock [AstBreak Nothing]) [] Nothing,
                AstBinaryFunc "=" (AstVar "i") (AstBinaryFunc "+" (AstVar "i") (AstInt 1))
              ])
            ], memory)
