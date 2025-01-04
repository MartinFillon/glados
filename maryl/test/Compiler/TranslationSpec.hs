{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TranslationSpec
-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.TranslationSpec (spec) where

import Compiler.ASTtoASM (translateAST, translateToASM)
import Parsing.ParserAst (Ast (..), MarylType (..), Variable (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import VirtualMachine.Instructions (Value (..), call, push, ret)

spec :: Spec
spec = do
    describe "translateAST" $ do
        it "translates AstInt to push instruction" $
            translateAST (AstInt 42) `shouldBe` [push Nothing (N 42)]

        it "translates AstBool to push instruction" $
            translateAST (AstBool True) `shouldBe` [push Nothing (B True)]

        it "translates AstString to push instruction" $
            translateAST (AstString "hello") `shouldBe` [push Nothing (S "hello")]

        it "translates AstDouble to push instruction" $
            translateAST (AstDouble 3.14) `shouldBe` [push Nothing (D 3.14)]

        it "translates AstChar to push instruction" $
            translateAST (AstChar 'a') `shouldBe` [push Nothing (S "a")]

        it "translates AstDefineVar for integer" $
            translateAST (AstDefineVar (Variable "x" Integer (AstInt 42))) `shouldBe` [push Nothing (N 42)]

        it "translates AstBinaryFunc for addition" $ do
            let ast = AstBinaryFunc "+" (AstInt 10) (AstInt 20)
            translateAST ast `shouldBe` [push Nothing (N 10), push Nothing (N 20), call Nothing "add"]

        it "translates AstReturn" $ do
            let ast = AstReturn (AstInt 42)
            translateAST ast `shouldBe` [push Nothing (N 42), ret Nothing]

    describe "translateToASM" $ do
        it "translates a list of Ast nodes" $ do
            let asts = [AstInt 10, AstBool False, AstString "test"]
            translateToASM asts
                `shouldBe` [ push Nothing (N 10),
                             push Nothing (B False),
                             push Nothing (S "test")
                           ]

-- it "translates a function definition" $ do
--   let ast = AstDefineFunc (Function "add" [] [AstBinaryFunc "+" (AstInt 1) (AstInt 2)] Integer)
--   translateAST ast `shouldBe`
--     [ pushArg Nothing 0,
--       pushArg Nothing 1,
--       push Nothing (N 1),
--       push Nothing (N 2),
--       call Nothing "add"
--     ]
