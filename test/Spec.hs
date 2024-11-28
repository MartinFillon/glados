{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

module Main (main) where

import PrinterSpec
import SExprParserSpec

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
    describe "SExpr Parser test" SExprParserSpec.spec
    describe "Eval Ast test" EvalAstSpec.spec
