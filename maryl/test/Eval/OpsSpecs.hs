{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- OpsSpecs
-}

module Eval.OpsSpecs (spec) where

import Eval.Evaluator (evalAST, evalNode)
import Memory (initMemory)
import Parsing.ParserAst (Ast (..), Function (..))
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "arithmetic operators" $ do
        context "evaluates addition" $ do
            it "addition on positive values" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstInt 1) (AstInt 2))
                    `shouldBe` Right (AstInt 3, initMemory)
            it "addition on positive and negative values" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstInt (-11)) (AstInt 2))
                    `shouldBe` Right (AstInt (-9), initMemory)
            it "addition on negative values" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstInt (-1)) (AstInt (-83)))
                    `shouldBe` Right (AstInt (-84), initMemory)
            it "addition on floats" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstDouble 1.0) (AstDouble 2.0))
                    `shouldBe` Right (AstDouble 3.0, initMemory)
            it "addition on floats" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstDouble 1.0) (AstDouble 2.0))
                    `shouldBe` Right (AstDouble 3.0, initMemory)
            it "addition on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstInt 10) (AstDouble 2.3))
                    `shouldBe` Right (AstDouble 12.3, initMemory)
            it "addition on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "+" (AstDouble 0.3) (AstInt (-1)))
                    `shouldBe` Right (AstDouble (-0.7), initMemory)

        context "evaluates division" $ do
            it "division with positive values" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstInt 4) (AstInt 2))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "division with positive and negative values" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstInt (-12)) (AstInt 4))
                    `shouldBe` Right (AstInt (-3), initMemory)
            it "division with floats" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstDouble 4.0) (AstDouble 2.0))
                    `shouldBe` Right (AstDouble 2.0, initMemory)
            it "division with negative values" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstInt (-4)) (AstInt (-2)))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "division to round up" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstInt (-4)) (AstInt 3))
                    `shouldBe` Right (AstInt (-1), initMemory)
            it "division by zero." $ do
                evalNode initMemory (AstBinaryFunc "/" (AstInt 4) (AstInt 0))
                    `shouldBe` Left "Division by zero."
            it "division by zero with floats" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstDouble 4.0) (AstDouble 0.0))
                    `shouldBe` Left "Division by zero."
            it "division on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstInt 25) (AstDouble 2.5))
                    `shouldBe` Right (AstDouble 10.0, initMemory)
            it "division on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "/" (AstDouble 4.8) (AstInt 2))
                    `shouldBe` Right (AstDouble 2.4, initMemory)

        context "evaluates multiplication" $ do
            it "multiplication with positive values" $ do
                evalNode initMemory (AstBinaryFunc "*" (AstInt 3) (AstInt 4))
                    `shouldBe` Right (AstInt 12, initMemory)
            it "multiplication with negative values" $ do
                evalNode initMemory (AstBinaryFunc "*" (AstInt (-3)) (AstInt (-4)))
                    `shouldBe` Right (AstInt 12, initMemory)
            it "multiplication with floats" $ do
                evalNode initMemory (AstBinaryFunc "*" (AstDouble 3.0) (AstDouble 4.0))
                    `shouldBe` Right (AstDouble 12.0, initMemory)
            it "multiplication with positive and negative values" $ do
                evalNode initMemory (AstBinaryFunc "*" (AstInt (-3)) (AstInt 4))
                    `shouldBe` Right (AstInt (-12), initMemory)
            it "multiplication on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "*" (AstInt 10) (AstDouble 2.3))
                    `shouldBe` Right (AstDouble 23.0, initMemory)
            it "multiplication on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "*" (AstDouble 0.3) (AstInt (-1)))
                    `shouldBe` Right (AstDouble (-0.3), initMemory)

        context "evaluates modulo" $ do
            it "modulo on positive values with %" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstInt 10) (AstInt 3))
                    `shouldBe` Right (AstInt 1, initMemory)
            it "modulo on positive values with mod" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstInt 10) (AstInt 4))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "modulo on negative values with %" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstInt (-10)) (AstInt 3))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "modulo by zero with %" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstInt 10) (AstInt 0))
                    `shouldBe` Left "Modulo by zero."
            it "modulo by zero with mod" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstInt 10) (AstInt 0))
                    `shouldBe` Left "Modulo by zero."
            it "modulo by zero with floats and %" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstDouble 10.0) (AstDouble 0.0))
                    `shouldBe` Left "Modulo by zero."
            it "modulo by zero with floats and mod" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstDouble 10.0) (AstDouble 0.0))
                    `shouldBe` Left "Modulo by zero."
            it "modulo on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstInt 10) (AstDouble 2.5))
                    `shouldBe` Right (AstDouble 0.0, initMemory)
            it "modulo on float and int values" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstDouble 4.5) (AstInt 25))
                    `shouldBe` Right (AstDouble 4.5, initMemory)
            it "modulo on floats with %" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstDouble 10.0) (AstDouble 3.0))
                    `shouldBe` Right (AstDouble 1.0, initMemory)
            it "modulo on floats with mod" $ do
                evalNode initMemory (AstBinaryFunc "%" (AstDouble 10.0) (AstDouble 4.0))
                    `shouldBe` Right (AstDouble 2.0, initMemory)

-- describe "predicates" $ do
--     context "evaluates equality" $ do
--         it "equal positive integers" $ do
--             evalNode initMemory (AstBinaryFunc "==" (AstInt 4) (AstInt 4))
--                 `shouldBe` Right (AstBool True, initMemory)
--         it "unequal positive integers" $ do
--             evalNode initMemory (AstBinaryFunc "==" (AstInt 4) (AstInt 5))
--                 `shouldBe` Right (AstBool False, initMemory)
--         it "equal negative integers" $ do
--             evalNode initMemory (AstBinaryFunc "==" (AstInt (-3)) (AstInt (-3)))
--                 `shouldBe` Right (AstBool True, initMemory)
--         it "unequal negative integers" $ do
--             evalNode initMemory (AstBinaryFunc "==" (AstInt (-3)) (AstInt (-4)))
--                 `shouldBe` Right (AstBool False, initMemory)
--         it "equal positive floats" $ do
--             evalNode initMemory (AstBinaryFunc "==" (AstDouble 4.0) (AstDouble 4.0))
--                 `shouldBe` Right (AstBool True, initMemory)
--         it "unequal positive floats" $ do
--             evalNode initMemory (AstBinaryFunc "==" (AstDouble 4.0) (AstDouble 5.0))
--                 `shouldBe` Right (AstBool False, initMemory)

        context "evaluates less than" $ do
            it "positive numbers comparison" $ do
                evalNode initMemory (AstBinaryFunc "<" (AstInt 3) (AstInt 5))
                    `shouldBe` Right (AstBool True, initMemory)
            it "negative and positive comparison" $ do
                evalNode initMemory (AstBinaryFunc "<" (AstInt (-3)) (AstInt 5))
                    `shouldBe` Right (AstBool True, initMemory)
            it "negative numbers comparison" $ do
                evalNode initMemory (AstBinaryFunc "<" (AstInt (-5)) (AstInt (-3)))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false case for comparison" $ do
                evalNode initMemory (AstBinaryFunc "<" (AstInt 5) (AstInt 3))
                    `shouldBe` Right (AstBool False, initMemory)
            it "positive float numbers comparison" $ do
                evalNode initMemory (AstBinaryFunc "<" (AstDouble 3.0) (AstDouble 5.0))
                    `shouldBe` Right (AstBool True, initMemory)

        context "evaluates greater than" $ do
            it "positive numbers comparison" $ do
                evalNode initMemory (AstBinaryFunc ">" (AstInt 7) (AstInt 2))
                    `shouldBe` Right (AstBool True, initMemory)
            it "negative and positive comparison" $ do
                evalNode initMemory (AstBinaryFunc ">" (AstInt (-2)) (AstInt (-7)))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false case for comparison" $ do
                evalNode initMemory (AstBinaryFunc ">" (AstInt (-3)) (AstInt (-2)))
                    `shouldBe` Right (AstBool False, initMemory)
            it "positive float numbers comparison" $ do
                evalNode initMemory (AstBinaryFunc ">" (AstDouble 7.0) (AstDouble 2.0))
                    `shouldBe` Right (AstBool True, initMemory)

        context "evaluates Boolean AND" $ do
            it "true and false" $ do
                evalNode initMemory (AstBinaryFunc "and" (AstBool True) (AstBool False))
                    `shouldBe` Right (AstBool False, initMemory)
            it "true and true" $ do
                evalNode initMemory (AstBinaryFunc "and" (AstBool True) (AstBool True))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false and false" $ do
                evalNode initMemory (AstBinaryFunc "and" (AstBool False) (AstBool False))
                    `shouldBe` Right (AstBool False, initMemory)
            it "error handling" $ do
                evalNode initMemory (AstBinaryFunc "and" (AstBool True) (AstInt 4))
                    `shouldBe` Left "Argument \"True\" invalid for \"and\"."

        context "evaluates Boolean OR" $ do
            it "true or false" $ do
                evalNode initMemory (AstBinaryFunc "or" (AstBool True) (AstBool False))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false or true" $ do
                evalNode initMemory (AstBinaryFunc "or" (AstBool False) (AstBool True))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false or false" $ do
                evalNode initMemory (AstBinaryFunc "or" (AstBool False) (AstBool False))
                    `shouldBe` Right (AstBool False, initMemory)
            it "error handling" $ do
                evalNode initMemory (AstBinaryFunc "or" (AstBool False) (AstInt (-4)))
                    `shouldBe` Left "Argument \"False\" invalid for \"or\"."
