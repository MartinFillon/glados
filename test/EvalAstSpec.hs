{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- EvalAstSpec
-}

module EvalAstSpec (spec) where

import Eval.Evaluator
import Parsing.SExprToAst
import Test.Hspec

spec :: Spec
spec = describe "evalAST" $ do
    context "evaluates literal integers" $ do
        it "positive integer" $ do
            evalAST (AstInt 42) `shouldBe` Just (AstInt 42)
        it "negative integer" $ do
            evalAST (AstInt (-42)) `shouldBe` Just (AstInt (-42))

    it "evaluates Boolean true" $ do
        evalAST (AstBool True) `shouldBe` Just (AstBool True)

    describe "arithmetic operators" $ do
        context "evaluates addition" $ do
            it "addition on positive values" $ do
                evalAST (Call (Function "+" [AstInt 1, AstInt 2]))
                    `shouldBe` Just (AstInt 3)
            it "addition on positive and negative values" $ do
                evalAST (Call (Function "+" [AstInt (-11), AstInt 2]))
                    `shouldBe` Just (AstInt (-9))
            it "addition on negative values" $ do
                evalAST (Call (Function "+" [AstInt (-1), AstInt (-83)]))
                    `shouldBe` Just (AstInt (-84))

        context "evaluates division" $ do
            it "division with positive values" $ do
                evalAST (Call (Function "/" [AstInt 4, AstInt 2]))
                    `shouldBe` Just (AstInt 2)
            it "division with positive and negative values" $ do
                evalAST (Call (Function "/" [AstInt (-12), AstInt 4]))
                    `shouldBe` Just (AstInt (-3))
            it "division with negative values" $ do
                evalAST (Call (Function "div" [AstInt (-4), AstInt (-2)]))
                    `shouldBe` Just (AstInt 2)
            it "division to round up" $ do
                evalAST (Call (Function "div" [AstInt (-4), AstInt 3]))
                    `shouldBe` Just (AstInt (-1))
            it "division by zero" $ do
                evalAST (Call (Function "/" [AstInt 4, AstInt 0]))
                    `shouldBe` Nothing

        context "evaluates multiplication" $ do
            it "multiplication with positive values" $ do
                evalAST (Call (Function "*" [AstInt 3, AstInt 4]))
                    `shouldBe` Just (AstInt 12)
            it "multiplication with negative values" $ do
                evalAST (Call (Function "*" [AstInt (-3), AstInt (-4)]))
                    `shouldBe` Just (AstInt 12)
            it "multiplication with positive and negative values" $ do
                evalAST (Call (Function "*" [AstInt (-3), AstInt 4]))
                    `shouldBe` Just (AstInt (-12))

        context "evaluates modulo" $ do
            it "modulo on positive values with %" $ do
                evalAST (Call (Function "%" [AstInt 10, AstInt 3]))
                    `shouldBe` Just (AstInt 1)
            it "modulo on positive values with mod" $ do
                evalAST (Call (Function "mod" [AstInt 10, AstInt 4]))
                    `shouldBe` Just (AstInt 2)
            it "modulo on negative values with %" $ do
                evalAST (Call (Function "%" [AstInt (-10), AstInt 3]))
                    `shouldBe` Just (AstInt 2)
            it "modulo on negative values with mod" $ do
                evalAST (Call (Function "mod" [AstInt (-10), AstInt (-4)]))
                    `shouldBe` Just (AstInt (-2))

    describe "predicates" $ do
        context "evaluates equality" $ do
            it "equal positive integers" $ do
                evalAST (Call (Function "eq?" [AstInt 4, AstInt 4]))
                    `shouldBe` Just (AstBool True)
            it "unequal positive integers" $ do
                evalAST (Call (Function "eq?" [AstInt 4, AstInt 5]))
                    `shouldBe` Just (AstBool False)
            it "equal negative integers" $ do
                evalAST (Call (Function "eq?" [AstInt (-3), AstInt (-3)]))
                    `shouldBe` Just (AstBool True)
            it "unequal negative integers" $ do
                evalAST (Call (Function "eq?" [AstInt (-3), AstInt (-4)]))
                    `shouldBe` Just (AstBool False)

        context "evaluates less than" $ do
            it "positive numbers comparison" $ do
                evalAST (Call (Function "<" [AstInt 3, AstInt 5]))
                    `shouldBe` Just (AstBool True)
            it "negative and positive comparison" $ do
                evalAST (Call (Function "<" [AstInt (-3), AstInt 5]))
                    `shouldBe` Just (AstBool True)
            it "negative numbers comparison" $ do
                evalAST (Call (Function "<" [AstInt (-5), AstInt (-3)]))
                    `shouldBe` Just (AstBool True)
            it "false case for comparison" $ do
                evalAST (Call (Function "<" [AstInt 5, AstInt 3]))
                    `shouldBe` Just (AstBool False)

        context "evaluates greater than" $ do
            it "positive numbers comparison" $ do
                evalAST (Call (Function ">" [AstInt 7, AstInt 2]))
                    `shouldBe` Just (AstBool True)
            it "negative and positive comparison" $ do
                evalAST (Call (Function ">" [AstInt (-2), AstInt (-7)]))
                    `shouldBe` Just (AstBool True)
            it "false case for comparison" $ do
                evalAST (Call (Function ">" [AstInt (-3), AstInt (-2)]))
                    `shouldBe` Just (AstBool False)

        context "evaluates Boolean AND" $ do
            it "true and false" $ do
                evalAST (Call (Function "and" [AstBool True, AstBool False]))
                    `shouldBe` Just (AstBool False)
            it "true and true" $ do
                evalAST (Call (Function "and" [AstBool True, AstBool True]))
                    `shouldBe` Just (AstBool True)
            it "false and false" $ do
                evalAST (Call (Function "and" [AstBool False, AstBool False]))
                    `shouldBe` Just (AstBool False)

        context "evaluates Boolean OR" $ do
            it "true or false" $ do
                evalAST (Call (Function "or" [AstBool True, AstBool False]))
                    `shouldBe` Just (AstBool True)
            it "false or true" $ do
                evalAST (Call (Function "or" [AstBool False, AstBool True]))
                    `shouldBe` Just (AstBool True)
            it "false or false" $ do
                evalAST (Call (Function "or" [AstBool False, AstBool False]))
                    `shouldBe` Just (AstBool False)

    describe "evaluates lambda calls" $ do
        context "basic lambda expressions" $ do
            it "simple addition lambda" $ do
                let lambdaExpr = Lambda ["x", "y"] (Call (Function "+" [AstSymbol "x" Nothing, AstSymbol "y" Nothing]))
                evalAST (Apply lambdaExpr [AstInt 10, AstInt 2])
                    `shouldBe` Just (AstInt 12)
            it "simple subtraction lambda" $ do
                let lambdaExpr = Lambda ["a", "b"] (Call (Function "-" [AstSymbol "a" Nothing, AstSymbol "b" Nothing]))
                evalAST (Apply lambdaExpr [AstInt 7, AstInt 5])
                    `shouldBe` Just (AstInt 2)

    it "evaluates define" $ do
        let defineExpr = Define "x" (AstInt 10)
        let symbolX = evalAST defineExpr
        evalAST (Call (Function "+" [AstSymbol "x" symbolX, AstInt 5]))
            `shouldBe` Just (AstInt 15)