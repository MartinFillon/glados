{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- LogicSpec
-}

module VirtualMachine.Operators.LogicSpec (spec) where

import Control.Exception (IOException)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)
import VirtualMachine.Instructions (
    Value (..),
    call,
    push,
    ret,
 )
import VirtualMachine.TestUtils (execTest, userError')

notEnoughArg :: String -> (IOException -> Bool) -> Spec
notEnoughArg s f = it ("should fail on " ++ s ++ " cause not enough arguments") $ do
    execTest
        [push Nothing (N 3), call Nothing s, ret Nothing]
        `shouldThrow` f

spec :: Spec
spec = do
    describe "Logic Operators Interpretation Spec" $ do
        it "should execute equality Hello == Hello" $
            do
                execTest
                    [ push Nothing (S "hELLO"),
                      push Nothing (S "hELLO"),
                      call Nothing "eq",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute nequality Hello /= Hello" $
            do
                execTest
                    [ push Nothing (S "p"),
                      push Nothing (S "hELLO"),
                      call Nothing "neq",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3 < 5" $
            do
                execTest
                    [ push Nothing (N 3),
                      push Nothing (N 5),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3.0 < 5" $
            do
                execTest
                    [ push Nothing (D 3.0),
                      push Nothing (N 5),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 3 < 5.0" $
            do
                execTest
                    [ push Nothing (N 3),
                      push Nothing (D 5.0),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute less than 4.0 < 50.0" $
            do
                execTest
                    [ push Nothing (D 4.0),
                      push Nothing (D 50.0),
                      call Nothing "less",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 10 > 2" $
            do
                execTest
                    [ push Nothing (N 10),
                      push Nothing (N 2),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 10.0 > 2" $
            do
                execTest
                    [ push Nothing (D 10.0),
                      push Nothing (N 2),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 10 > 3.0" $
            do
                execTest
                    [ push Nothing (N 10),
                      push Nothing (D 3.0),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute greater than 25.5 > 0.2" $
            do
                execTest
                    [ push Nothing (D 25.5),
                      push Nothing (D 0.2),
                      call Nothing "greater",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute logical and true && false" $
            do
                execTest
                    [ push Nothing (B True),
                      push Nothing (B False),
                      call Nothing "and",
                      ret Nothing
                    ]
                `shouldReturn` B False

        it "should execute logical or true || false" $
            do
                execTest
                    [ push Nothing (B True),
                      push Nothing (B False),
                      call Nothing "or",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute logical or false || true" $
            do
                execTest
                    [ push Nothing (B False),
                      push Nothing (B True),
                      call Nothing "or",
                      ret Nothing
                    ]
                `shouldReturn` B True

        it "should execute logical not" $
            do
                execTest
                    [ push Nothing (B True),
                      call Nothing "not",
                      ret Nothing
                    ]
                `shouldReturn` B False

        notEnoughArg "and" (userError' "And expects two booleans")
        notEnoughArg "greater" (userError' "Greater expects two number")
        notEnoughArg "less" (userError' "Lesser expects two number")
        notEnoughArg "or" (userError' "Or expects two booleans")
        notEnoughArg "not" (userError' "Not expects two booleans")
        notEnoughArg "eq" (userError' "Eq expects two value")
        notEnoughArg "neq" (userError' "Neq expects two value")

        it "should execute mixed comparison 3.14 > 3" $
            do
                execTest
                    [push Nothing (D 3.14), push Nothing (N 3), call Nothing "greater", ret Nothing]
                `shouldReturn` B True
