{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- SExprToAstSpec
-}

module SExprToAstSpec (spec) where

import Parsing.ParserSExpr
import Parsing.SExprToAst
import Test.Hspec

spec :: Spec
spec = do
    describe "Ast Evaluation" $ do
        context "evalPlus" $
            it "should be Just (Value (Number 2))" $
                evalPlus [Value (Number 1), Value (Number 1)] `shouldBe` Just (Value (Number 2))
        context "evalMinus" $
            it "should be Just (Value (Number 2))" $
                evalMinus [Value (Number 3), Value (Number 1)] `shouldBe` Just (Value (Number 2))

        context "evalMul" $
            it "should be Just (Value (Number 3))" $
                evalMul [Value (Number 3), Value (Number 1)] `shouldBe` Just (Value (Number 3))

        context "evalDiv" $
            it "should be Just (Value (Number 3))" $
                evalDiv [Value (Number 3), Value (Number 1)] `shouldBe` Just (Value (Number 3))

        context "evalMod" $
            it "should be Just (Value (Number 1))" $
                evalMod [Value (Number 3), Value (Number 2)] `shouldBe` Just (Value (Number 1))

        context "evalEq" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Number 3), Value (Number 3)] `shouldBe` Just (Value (Bool True))

        context "evalInf" $
            it "should be Just (Value (Bool True))" $
                evalInf [Value (Number 1), Value (Number 2)] `shouldBe` Just (Value (Bool True))

        context "evalIf" $
            it "should be Just (Value (Number 1))" $
                evalIf [Value (Bool True), Value (Number 1), Value (Number 2)]
                    `shouldBe` Just
                        (Value (Number 1))
        it
            "should be Just (Value (Number 2))"
            $ evalIf [Value (Bool False), Value (Number 1), Value (Number 2)] `shouldBe` Just (Value (Number 2))

        context "floating point add" $
            it "should be Just (Value (Float 2.0))" $
                evalPlus [Value (Float 1.0), Value (Float 1.0)] `shouldBe` Just (Value (Float 2.0))

        context "floating point and number add" $
            it "should be Just (Value (Float 2.0))" $
                evalPlus [Value (Float 1.0), Value (Number 1)] `shouldBe` Just (Value (Float 2.0))

        context "number float equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Float 1.0), Value (Float 1.0)] `shouldBe` Just (Value (Bool True))

        context "number float equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Float 1.0), Value (Number 1)] `shouldBe` Just (Value (Bool True))

        context "number bool equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Number 1), Value (Bool True)] `shouldBe` Just (Value (Bool True))

        context "float bool equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Bool True), Value (Float 1.0)] `shouldBe` Just (Value (Bool True))

        context "bool number equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Bool True), Value (Number 1)] `shouldBe` Just (Value (Bool True))

        context "bool float equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Bool True), Value (Float 1.0)] `shouldBe` Just (Value (Bool True))

        context "number float equals" $
            it "should be Just (Value (Bool True))" $
                evalEq [Value (Number 1), Value (Float 1.0)] `shouldBe` Just (Value (Bool True))
