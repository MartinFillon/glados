{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

import Test.Hspec

import PrinterSpec
import SExprParserSpec

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
    describe "SExpr Parser test" SExprParserSpec.spec
