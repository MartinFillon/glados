{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec (describe, hspec)

import EvalAstSpec (spec)
import PrinterSpec (spec)
import SExprParserSpec (spec)
import SExprToAstSpec (spec)

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
    describe "SExpr Parser test" SExprParserSpec.spec
    describe "Eval Ast test" EvalAstSpec.spec
    describe "SExprToAst test" SExprToAstSpec.spec
