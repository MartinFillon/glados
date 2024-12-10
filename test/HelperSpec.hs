{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- HelperSpec
-}

module HelperSpec (shouldBeApproximatelyAst) where

import Parsing.SExprToAst (Ast (..))
import Test.Hspec (Expectation, expectationFailure, shouldBe)

shouldBeApproximatelyAst :: Ast -> Ast -> Expectation
shouldBeApproximatelyAst (AstFloat x) (AstFloat y) =
    abs (x - y) < 1e-6 `shouldBe` True
shouldBeApproximatelyAst _ _ = expectationFailure "Not both AstFloat"
