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
import VirtualMachine.ParserSpec (spec)

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
    describe "SExpr Parser test" SExprParserSpec.spec
    describe "Eval Ast test" EvalAstSpec.spec
    describe "SExprToAst test" SExprToAstSpec.spec
    describe "VirtualMachine Parser spec" VirtualMachine.ParserSpec.spec
