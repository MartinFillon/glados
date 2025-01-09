{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TranslationSpec
-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.TranslationSpec (spec) where

import Compiler.ASTtoASM (translateAST, translateToASM)
import Memory (initMemory)
import Parsing.ParserAst (Ast (..), MarylType (..), Variable (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import VirtualMachine.Instructions (Value (..), call, push, ret)

spec :: Spec
spec = do
    describe "translateAST" $ do
        it "translates AstInt to push instruction" $
            fst (translateAST (AstInt 42) initMemory) `shouldBe` [push Nothing (N 42)]

        it "translates AstBool to push instruction" $
            fst (translateAST (AstBool True) initMemory) `shouldBe` [push Nothing (B True)]

        it "translates AstString to push instruction" $
            fst (translateAST (AstString "hello") initMemory)
                `shouldBe` [push Nothing (S "hello")]

        it "translates AstDouble to push instruction" $
            fst (translateAST (AstDouble 3.14) initMemory)
                `shouldBe` [push Nothing (D 3.14)]

        it "translates AstChar to push instruction" $
            fst (translateAST (AstChar 'a') initMemory) `shouldBe` [push Nothing (S "a")]

        it "translates AstDefineVar for integer" $
            fst (translateAST (AstDefineVar (Variable "x" Int (AstInt 42))) initMemory) `shouldBe` []

        it "translates AstBinaryFunc for addition" $ do
            let ast = AstBinaryFunc "+" (AstInt 10) (AstInt 20)
            fst (translateAST ast initMemory)
                `shouldBe` [push Nothing (N 10), push Nothing (N 20), call Nothing "add"]

        it "translates AstReturn" $ do
            let ast = AstReturn (AstInt 42)
            fst (translateAST ast initMemory) `shouldBe` [push Nothing (N 42), ret Nothing]

    describe "translateToASM" $ do
        it "translates a list of Ast nodes" $ do
            let asts = [AstInt 10, AstBool False, AstString "test"]
            translateToASM asts initMemory
                `shouldBe` ([ push Nothing (N 10),
                             push Nothing (B False),
                             push Nothing (S "test")
                           ], initMemory)

-- it "translates a function definition" $ do
--   let ast = AstDefineFunc (Function "add" [] [AstBinaryFunc "+" (AstInt 1) (AstInt 2)] Int)
--   translateAST ast `shouldBe`
--     [ pushArg Nothing 0,
--       pushArg Nothing 1,
--       push Nothing (N 1),
--       push Nothing (N 2),
--       call Nothing "add"
--     ]
