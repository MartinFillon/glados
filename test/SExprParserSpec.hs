module SExprParserSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)
import Parsing.ParserSExpr (parseSexpr)
import Data.SExpresso.SExpr (SExpr(SList, SAtom))
import Data.Either (isLeft)

spec :: Spec
spec = do
    describe "parsing string expression to object" $ do
        context "basic valid addition" $ it "\"(+ 1 1)\" should be Right (SList () [SAtom \"+\", SAtom \"1\", SAtom \"1\"])" $ parseSexpr "(+ 1 1)" `shouldBe` Right (SList () [SAtom "+", SAtom "1", SAtom "1"])
        context "basic valid subtraction" $ it "\"(- 1 -1)\" should be Right (SList () [SAtom \"-\", SAtom \"1\", SAtom \"-1\"])" $ parseSexpr "(- 1 -1)" `shouldBe` Right (SList () [SAtom "-", SAtom "1", SAtom "-1"])
        context "basic valid multiplication" $ it "\"(* +1 1)\" should be Right (SList () [SAtom \"*\", SAtom \"+1\", SAtom \"1\"])" $ parseSexpr "(* +1 1)" `shouldBe` Right (SList () [SAtom "*", SAtom "+1", SAtom "1"])
        context "basic valid division" $ it "\"(div 1 1)\" should be Right (SList () [SAtom \"div\", SAtom \"1\", SAtom \"1\"])" $ parseSexpr "(div 1 1)" `shouldBe` Right (SList () [SAtom "div", SAtom "1", SAtom "1"])
        context "basic valid modulo" $ it "\"(mod 1 1)\" should be Right (SList () [SAtom \"mod\", SAtom \"1\", SAtom \"1\"])" $ parseSexpr "(mod 1 1)" `shouldBe` Right (SList () [SAtom "mod", SAtom "1", SAtom "1"])
        context "basic valid if" $ it "\"(if #t 1 2)\" should be Right (SList () [SAtom \"if\", SAtom \"#t\", SAtom \"1\", SAtom \"2\"])" $ parseSexpr "(if #t 1 2)" `shouldBe` Right (SList () [SAtom "if", SAtom "#t", SAtom "1", SAtom "2"])
        context "basic valid equality" $ it "\"(eq? foo #f)\" should be Right (SList () [SAtom \"eq?\", SAtom \"foo\", SAtom \"#f\"])" $ parseSexpr "(eq? foo #f)" `shouldBe` Right (SList () [SAtom "eq?", SAtom "foo", SAtom "#f"])
        context "basic valid lower comparison" $ it "\"(< 9 15)\" should be Right (SList () [SAtom \"<\", SAtom \"9\", SAtom \"15\"])" $ parseSexpr "(< 9 15)" `shouldBe` Right (SList () [SAtom "<", SAtom "9", SAtom "15"])
        context "valid variable name with numbers" $ it "\"(+ 2 foo123)\" should be Right (SList () [SAtom \"+\", SAtom \"2\", SAtom \"foo123\"])" $ parseSexpr "(+ 2 foo123)" `shouldBe` Right (SList () [SAtom "+", SAtom "2", SAtom "foo123"])
        context "invalid input: no closing parenthesis" $ it "\"(+ 2 foo\" should be Left" $ isLeft (parseSexpr "(+ 2 foo") `shouldBe` True
        context "invalid input: no opening parenthesis" $ it "\"+ 2 foo)\" should be Left" $ isLeft (parseSexpr "+ 2 foo)") `shouldBe` True
        context "invalid input: invalid variable name" $ it "\"(+ 2 123foo)\" should be Left" $ isLeft (parseSexpr "(+ 2 123foo)") `shouldBe` True
