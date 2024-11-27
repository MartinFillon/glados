{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

module Main (main) where

import qualified PrinterSpec
import qualified EvalAstSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
    PrinterSpec.spec
    EvalAstSpec.spec

-- Parsing.SExprToAst.spec
-- Template.spec
