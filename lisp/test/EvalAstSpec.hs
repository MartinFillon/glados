{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- EvalAst initMemorySpec
-}

module EvalAstSpec (spec) where

import Eval.Evaluator (evalAST)
import Eval.Maths (isNumeric)
import HelperSpec (shouldBeApproximatelyAst)
import Memory (initMemory)
import Parsing.SExprToAst (Ast (..), Function (..))
import Test.Hspec (Spec, context, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec = describe "evalAST initMemory" $ do
    context "evaluates literal integers" $ do
        it "positive integer" $ do
            evalAST initMemory (AstInt 42) `shouldBe` Right (AstInt 42, initMemory)
        it "negative integer" $ do
            evalAST initMemory (AstInt (-42)) `shouldBe` Right (AstInt (-42), initMemory)

    context "evaluates literal floats" $ do
        it "float" $ do
            let result = evalAST initMemory (AstFloat 42.0)
            case result of
                Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 42.0)
                _ -> expectationFailure "Expected AstFloat result"
        it "negative float" $ do
            let result = evalAST initMemory (AstFloat (-42.0))
            case result of
                Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat (-42.0))
                _ -> expectationFailure "Expected AstFloat result"

    context "evaluates literal booleans" $ do
        it "evaluates Boolean true" $ do
            evalAST initMemory (AstBool True) `shouldBe` Right (AstBool True, initMemory)
        it "evaluates Boolean false" $ do
            evalAST initMemory (AstBool False) `shouldBe` Right (AstBool False, initMemory)

    describe "arithmetic operators" $ do
        context "evaluates addition" $ do
            it "addition on positive values" $ do
                evalAST initMemory (Call (Function "+" [AstInt 1, AstInt 2]))
                    `shouldBe` Right (AstInt 3, initMemory)
            it "addition on positive and negative values" $ do
                evalAST initMemory (Call (Function "+" [AstInt (-11), AstInt 2]))
                    `shouldBe` Right (AstInt (-9), initMemory)
            it "addition on negative values" $ do
                evalAST initMemory (Call (Function "+" [AstInt (-1), AstInt (-83)]))
                    `shouldBe` Right (AstInt (-84), initMemory)
            it "addition on floats" $ do
                let result = evalAST initMemory (Call (Function "+" [AstFloat 1.0, AstFloat 2.0]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 3.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "addition on float and int values" $ do
                let result = evalAST initMemory (Call (Function "+" [AstInt 10, AstFloat 2.3]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 12.3)
                    _ -> expectationFailure "Expected AstFloat result"
            it "addition on float and int values" $ do
                let result = evalAST initMemory (Call (Function "+" [AstFloat 0.3, AstInt (-1)]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat (-0.7))
                    _ -> expectationFailure "Expected AstFloat result"

        context "evaluates division" $ do
            it "division with positive values" $ do
                evalAST initMemory (Call (Function "/" [AstInt 4, AstInt 2]))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "division with positive and negative values" $ do
                evalAST initMemory (Call (Function "/" [AstInt (-12), AstInt 4]))
                    `shouldBe` Right (AstInt (-3), initMemory)
            it "division with floats" $ do
                let result = evalAST initMemory (Call (Function "/" [AstFloat 4.0, AstFloat 2.0]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 2.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "division with negative values" $ do
                evalAST initMemory (Call (Function "div" [AstInt (-4), AstInt (-2)]))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "division to round up" $ do
                evalAST initMemory (Call (Function "div" [AstInt (-4), AstInt 3]))
                    `shouldBe` Right (AstInt (-1), initMemory)
            it "division by zero" $ do
                evalAST initMemory (Call (Function "/" [AstInt 4, AstInt 0]))
                    `shouldBe` Left "Division by zero"
            it "division by zero with floats" $ do
                evalAST initMemory (Call (Function "/" [AstFloat 4.0, AstFloat 0.0]))
                    `shouldBe` Left "Division by zero"
            it "division on float and int values" $ do
                let result = evalAST initMemory (Call (Function "div" [AstInt 25, AstFloat 2.5]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 10.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "division on float and int values" $ do
                let result = evalAST initMemory (Call (Function "div" [AstFloat 4.8, AstInt 2]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 2.4)
                    _ -> expectationFailure "Expected AstFloat result"

        context "evaluates multiplication" $ do
            it "multiplication with positive values" $ do
                evalAST initMemory (Call (Function "*" [AstInt 3, AstInt 4]))
                    `shouldBe` Right (AstInt 12, initMemory)
            it "multiplication with negative values" $ do
                evalAST initMemory (Call (Function "*" [AstInt (-3), AstInt (-4)]))
                    `shouldBe` Right (AstInt 12, initMemory)
            it "multiplication with floats" $ do
                let result = evalAST initMemory (Call (Function "*" [AstFloat 3.0, AstFloat 4.0]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 12.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "multiplication with positive and negative values" $ do
                evalAST initMemory (Call (Function "*" [AstInt (-3), AstInt 4]))
                    `shouldBe` Right (AstInt (-12), initMemory)
            it "multiplication on float and int values" $ do
                let result = evalAST initMemory (Call (Function "*" [AstInt 10, AstFloat 2.3]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 23.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "multiplication on float and int values" $ do
                let result = evalAST initMemory (Call (Function "*" [AstFloat 0.3, AstInt (-1)]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat (-0.3))
                    _ -> expectationFailure "Expected AstFloat result"

        context "evaluates modulo" $ do
            it "modulo on positive values with %" $ do
                evalAST initMemory (Call (Function "%" [AstInt 10, AstInt 3]))
                    `shouldBe` Right (AstInt 1, initMemory)
            it "modulo on positive values with mod" $ do
                evalAST initMemory (Call (Function "mod" [AstInt 10, AstInt 4]))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "modulo on negative values with %" $ do
                evalAST initMemory (Call (Function "%" [AstInt (-10), AstInt 3]))
                    `shouldBe` Right (AstInt 2, initMemory)
            it "modulo by zero with %" $ do
                evalAST initMemory (Call (Function "%" [AstInt 10, AstInt 0]))
                    `shouldBe` Left "Modulo by zero"
            it "modulo by zero with mod" $ do
                evalAST initMemory (Call (Function "mod" [AstInt 10, AstInt 0]))
                    `shouldBe` Left "Modulo by zero"
            it "modulo by zero with floats and %" $ do
                evalAST initMemory (Call (Function "%" [AstFloat 10.0, AstFloat 0.0]))
                    `shouldBe` Left "Modulo by zero"
            it "modulo by zero with floats and mod" $ do
                evalAST initMemory (Call (Function "mod" [AstFloat 10.0, AstFloat 0.0]))
                    `shouldBe` Left "Modulo by zero"
            it "modulo on float and int values" $ do
                let result = evalAST initMemory (Call (Function "mod" [AstInt 10, AstFloat 2.5]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 0.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "modulo on float and int values" $ do
                let result = evalAST initMemory (Call (Function "mod" [AstFloat 4.5, AstInt 25]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 4.5)
                    _ -> expectationFailure "Expected AstFloat result"
            it "modulo on floats with %" $ do
                let result = evalAST initMemory (Call (Function "%" [AstFloat 10.0, AstFloat 3.0]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 1.0)
                    _ -> expectationFailure "Expected AstFloat result"
            it "modulo on floats with mod" $ do
                let result = evalAST initMemory (Call (Function "mod" [AstFloat 10.0, AstFloat 4.0]))
                case result of
                    Right (AstFloat x, _) -> shouldBeApproximatelyAst (AstFloat x) (AstFloat 2.0)
                    _ -> expectationFailure "Expected AstFloat result"

    describe "predicates" $ do
        context "evaluates equality" $ do
            it "equal positive integers" $ do
                evalAST initMemory (Call (Function "eq?" [AstInt 4, AstInt 4]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "unequal positive integers" $ do
                evalAST initMemory (Call (Function "eq?" [AstInt 4, AstInt 5]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "equal negative integers" $ do
                evalAST initMemory (Call (Function "eq?" [AstInt (-3), AstInt (-3)]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "unequal negative integers" $ do
                evalAST initMemory (Call (Function "eq?" [AstInt (-3), AstInt (-4)]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "equal positive floats" $ do
                evalAST initMemory (Call (Function "eq?" [AstFloat 4.0, AstFloat 4.0]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "unequal positive floats" $ do
                evalAST initMemory (Call (Function "eq?" [AstFloat 4.0, AstFloat 5.0]))
                    `shouldBe` Right (AstBool False, initMemory)

        context "evaluates less than" $ do
            it "positive numbers comparison" $ do
                evalAST initMemory (Call (Function "<" [AstInt 3, AstInt 5]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "negative and positive comparison" $ do
                evalAST initMemory (Call (Function "<" [AstInt (-3), AstInt 5]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "negative numbers comparison" $ do
                evalAST initMemory (Call (Function "<" [AstInt (-5), AstInt (-3)]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false case for comparison" $ do
                evalAST initMemory (Call (Function "<" [AstInt 5, AstInt 3]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "positive float numbers comparison" $ do
                evalAST initMemory (Call (Function "<" [AstFloat 3.0, AstFloat 5.0]))
                    `shouldBe` Right (AstBool True, initMemory)

        context "evaluates greater than" $ do
            it "positive numbers comparison" $ do
                evalAST initMemory (Call (Function ">" [AstInt 7, AstInt 2]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "negative and positive comparison" $ do
                evalAST initMemory (Call (Function ">" [AstInt (-2), AstInt (-7)]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false case for comparison" $ do
                evalAST initMemory (Call (Function ">" [AstInt (-3), AstInt (-2)]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "positive float numbers comparison" $ do
                evalAST initMemory (Call (Function ">" [AstFloat 7.0, AstFloat 2.0]))
                    `shouldBe` Right (AstBool True, initMemory)

        context "evaluates Boolean AND" $ do
            it "true and false" $ do
                evalAST initMemory (Call (Function "and" [AstBool True, AstBool False]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "true and true" $ do
                evalAST initMemory (Call (Function "and" [AstBool True, AstBool True]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false and false" $ do
                evalAST initMemory (Call (Function "and" [AstBool False, AstBool False]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "error handling" $ do
                evalAST initMemory (Call (Function "and" [AstBool True, AstInt 4]))
                    `shouldBe` Left "Arguments #t out of bound for `and`"

        context "evaluates Boolean OR" $ do
            it "true or false" $ do
                evalAST initMemory (Call (Function "or" [AstBool True, AstBool False]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false or true" $ do
                evalAST initMemory (Call (Function "or" [AstBool False, AstBool True]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "false or false" $ do
                evalAST initMemory (Call (Function "or" [AstBool False, AstBool False]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "error handling" $ do
                evalAST initMemory (Call (Function "or" [AstBool False, AstInt (-4)]))
                    `shouldBe` Left "Arguments #f out of bound for `or`"

        context "evaluates Boolean NOT" $ do
            it "true" $ do
                evalAST initMemory (Call (Function "not" [AstBool True]))
                    `shouldBe` Right (AstBool False, initMemory)
            it "false" $ do
                evalAST initMemory (Call (Function "not" [AstBool False]))
                    `shouldBe` Right (AstBool True, initMemory)
            it "error handling" $ do
                evalAST initMemory (Call (Function "not" [AstBool False, AstInt (-4)]))
                    `shouldBe` Left "Invalid arguments [#f,-4] for `not`"

    describe "evaluates lambda calls" $ do
        context "basic lambda expressions" $ do
            it "simple addition lambda" $ do
                let lambdaExpr =
                        Lambda
                            ["x", "y"]
                            (Call (Function "+" [AstSymbol "x" AstVoid, AstSymbol "y" AstVoid]))
                evalAST initMemory (Apply lambdaExpr [AstInt 10, AstInt 2])
                    `shouldBe` Right (AstInt 12, initMemory)
            it "simple subtraction lambda" $ do
                let lambdaExpr =
                        Lambda
                            ["a", "b"]
                            (Call (Function "-" [AstSymbol "a" AstVoid, AstSymbol "b" AstVoid]))
                evalAST initMemory (Apply lambdaExpr [AstInt 7, AstInt 5])
                    `shouldBe` Right (AstInt 2, initMemory)

    describe "variable definitions" $ do
        it "defines and evaluates a variable" $ do
            let defineExpr = Define "foo" (AstInt 5)
            let memoryAfterDefine = case evalAST initMemory defineExpr of
                    Right (_, mem) -> mem
                    _ -> error "Definition failed"
            evalAST memoryAfterDefine (AstSymbol "foo" AstVoid)
                `shouldBe` Right (AstInt 5, memoryAfterDefine)

    describe "evaluate condition" $ do
        it "evaluates condition true" $ do
            let conditionExpr = Condition (Function "if" [AstBool True, AstInt 10, AstInt 5])
            evalAST initMemory conditionExpr `shouldBe` Right (AstInt 10, initMemory)
        it "evaluates condition false" $ do
            let conditionExpr = Condition (Function "if" [AstBool False, AstInt 10, AstInt 42])
            evalAST initMemory conditionExpr `shouldBe` Right (AstInt 42, initMemory)

    describe "recursive compter" $ do
        it "stops at 0" $ do
            let recursiveCounter =
                    Lambda
                        ["x"]
                        ( Condition
                            ( Function
                                "if"
                                [ Call (Function "eq?" [AstSymbol "x" AstVoid, AstInt 0]),
                                  AstInt 0,
                                  Call (Function "self" [Call (Function "-" [AstSymbol "x" AstVoid, AstInt 1])])
                                ]
                            )
                        )
            let memoryAfterDefine = case evalAST initMemory (Define "self" recursiveCounter) of
                    Right (_, mem) -> mem
                    _ -> error "Definition failed"
            case evalAST memoryAfterDefine (Call (Function "self" [AstInt 5])) of
                Right (x, _) -> x `shouldBe` AstInt 0
                _ -> expectationFailure "Expected AstInt 0"

    describe "util functions" $ do
        context "checks for numeric types in AST" $ do
            it "is an int numeric" $ do
                isNumeric (AstInt 3) `shouldBe` True
            it "is a float numeric" $ do
                isNumeric (AstFloat 3.4) `shouldBe` True
            it "is not a numeric" $ do
                isNumeric (AstBool True) `shouldBe` False
