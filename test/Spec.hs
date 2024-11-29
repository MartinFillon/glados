{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec

import PrinterSpec
import SExprParserSpec
import EvalAstSpec
import SExprToAstSpec

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
    describe "SExpr Parser test" SExprParserSpec.spec
    describe "Eval Ast test" EvalAstSpec.spec
    describe "SExpr to AST test" SExprToAstSpec.spec
