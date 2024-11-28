{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- EvalAstSpec
-}

module EvalAstSpec (spec) where

import Parsing.SExprToAst
import Test.Hspec

spec :: Spec
spec = describe "evalAST" $ do
    it "evaluates literal integers" $ do
        evalAST (AstInt 42) `shouldBe` Just (AstInt 42)

    it "evaluates Boolean true" $ do
        evalAST (AstBool True) `shouldBe` Just (AstBool True)

    it "evaluates addition" $ do
        evalAST (Call (Function "+" [AstInt 1, AstInt 2]))
            `shouldBe` Just (AstInt 3)

    it "evaluates multiplication" $ do
        evalAST (Call (Function "*" [AstInt 3, AstInt 4]))
            `shouldBe` Just (AstInt 12)

    it "evaluates Boolean AND" $ do
        evalAST (Call (Function "and" [AstBool True, AstBool False]))
            `shouldBe` Just (AstBool False)

    it "evaluates lambda call" $ do
        let lambdaExpr = Lambda ["x", "y"] (Call (Function "/" [AstSymbol "x" Nothing, AstSymbol "y" Nothing]))
        evalAST (Apply lambdaExpr [AstInt 10, AstInt 2])
            `shouldBe` Just (AstInt 5)

    it "evaluates define" $ do
        let defineExpr = Define "x" (AstInt 10)
        let symbolX = evalAST defineExpr
        evalAST (Call (Function "+" [AstSymbol "x" symbolX, AstInt 5]))
            `shouldBe` Just (AstInt 15)

    it "evaluates modulo operation" $ do
        evalAST (Call (Function "%" [AstInt 10, AstInt 3]))
            `shouldBe` Just (AstInt 1)

    it "evaluates less than" $ do
        evalAST (Call (Function "<" [AstInt 3, AstInt 5]))
            `shouldBe` Just (AstBool True)

    it "evaluates equals" $ do
        evalAST (Call (Function "eq?" [AstInt 4, AstInt 4]))
            `shouldBe` Just (AstBool True)

    it "evaluates greater than" $ do
        evalAST (Call (Function ">" [AstInt 7, AstInt 2]))
            `shouldBe` Just (AstBool True)