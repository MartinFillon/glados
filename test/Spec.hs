{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

import Test.Hspec

import PrinterSpec

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
