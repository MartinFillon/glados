{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec (describe, hspec)

import Compiler.SerializeSpec (spec)
import Compiler.TranslationSpec (spec)
import PrinterSpec (spec)
import VirtualMachine.InterpreterSpec (spec)
import VirtualMachine.ParserSpec (spec)

main :: IO ()
main = hspec $ do
    describe "Printer test" PrinterSpec.spec
    describe "VirtualMachine Parser spec" VirtualMachine.ParserSpec.spec
    describe "VirtualMachine Interpreter spec" VirtualMachine.InterpreterSpec.spec
    describe "Compiler Translation spec" Compiler.TranslationSpec.spec
    describe "Compiler Serializing spec" Compiler.SerializeSpec.spec
