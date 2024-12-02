{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- SExprParserSpec
-}

module SExprParserSpec (spec) where

import Data.Either (isLeft)
import Parsing.ParserSExpr (Atom (..), ParserError, Sexpr (..), parseSexpr)
import Test.Hspec (Spec, context, describe, it, shouldBe)

parseSexpr' :: String -> Either ParserError (Sexpr Int Double)
parseSexpr' = parseSexpr

spec :: Spec
spec = do
    describe "parsing string expression to object" $ do
        context "basic valid addition" $
            it "\"(+ 1 1)\" should be Right (List [Atom (String \"+\"), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(+ 1 1)" `shouldBe` Right (List [Atom (String "+"), Atom (Number 1), Atom (Number 1)])

        context "basic valid subtraction" $
            it "\"(- 1 -1)\" should be Right (List [Atom (String \"-\"), Atom (Number 1), Atom (Number (-1)])" $
                parseSexpr' "(- 1 -1)" `shouldBe` Right (List [Atom (String "-"), Atom (Number 1), Atom (Number (-1))])

        context "basic valid multiplication" $
            it "\"(* +1 1)\" should be Right (List [Atom (String \"*\"), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(* +1 1)" `shouldBe` Right (List [Atom (String "*"), Atom (Number 1), Atom (Number 1)])

        context "basic valid division" $
            it "\"(div 1 1)\" should be Right (List [Atom (String \"div\"), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(div 1 1)" `shouldBe` Right (List [Atom (String "div"), Atom (Number 1), Atom (Number 1)])

        context "basic valid comparison" $
            it "\"(< 1 1)\" should be Right (List [Atom (String \"<\"), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(< 1 1)" `shouldBe` Right (List [Atom (String "<"), Atom (Number 1), Atom (Number 1)])

        context "basic valid equality" $
            it "\"(eq? 1 1)\" should be Right (List [Atom (String \"eq?\"), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(eq? 1 1)" `shouldBe` Right (List [Atom (String "eq?"), Atom (Number 1), Atom (Number 1)])

        context "basic valid if" $
            it "\"(if 1 1 1)\" should be Right (List [Atom (String \"if\"), Atom (Number 1), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(if 1 1 1)" `shouldBe` Right (List [Atom (String "if"), Atom (Number 1), Atom (Number 1), Atom (Number 1)])

        context "basic valid modulo" $
            it "\"(mod 1 1)\" should be Right (List [Atom (String \"mod\"), Atom (Number 1), Atom (Number 1)])" $
                parseSexpr' "(mod 1 1)" `shouldBe` Right (List [Atom (String "mod"), Atom (Number 1), Atom (Number 1)])

        context "valid multiple lists" $
            it "\"(x (x 1 2 3 (A b))(a B)) should be Right (List [Atom (String \"x\"), List [Atom (String \"x\"), Atom (Number 1), Atom (Number 2), Atom (Number 3), List [Atom (String \"A\"), Atom (String \"b\")]], List [Atom (String \"a\"), Atom (String \"B\")])" $
                parseSexpr' "(x (x 1 2 3 (A b))(a B))" `shouldBe` Right (List [Atom (String "x"), List [Atom (String "x"), Atom (Number 1), Atom (Number 2), Atom (Number 3), List [Atom (String "A"), Atom (String "b")]], List [Atom (String "a"), Atom (String "B")]])

        context "Basic float" $
            it "\"(+ 1.0 1.0)\" should be Right (List [Atom (String \"+\"), Atom (Float 1.0), Atom (Float 1.0)])" $
                parseSexpr' "(+ 1.0 1.0)" `shouldBe` Right (List [Atom (String "+"), Atom (Float 1.0), Atom (Float 1.0)])

        context "Invalid expression" $
            it "\"(+ 1 1\" should be Left \"unexpected end of input\"" $
                isLeft (parseSexpr' "(+ 1 1") `shouldBe` True

        context "Simple bool" $
            it "\"(#t)\" should be Right (Bool True)" $
                parseSexpr' "(#t)"
                    `shouldBe` Right
                        (List [Atom (Bool True)])
        context "Simple bool" $
            it "\"(#f)\" should be Right (Bool False)" $
                parseSexpr' "(#f)" `shouldBe` Right (List [Atom (Bool False)])
