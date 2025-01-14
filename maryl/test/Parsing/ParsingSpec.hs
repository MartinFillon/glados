{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ParsingSpec
-}

module Parsing.ParsingSpec (spec) where

import Data.Either (isLeft)
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..), ParserError, Variable (..), parseAST)
import Test.Hspec (Spec, context, describe, it, shouldBe)

parseAST' :: String -> Either ParserError [Ast]
parseAST' = parseAST

spec :: Spec
spec = do
  describe "Parsing Maryl language constructs" $ do
    context "Variable declaration" $ do
      it "int foo = 42;" $ do
        parseAST' "int foo = 42;"
          `shouldBe` Right [AstDefineVar (Variable "foo" Int (AstInt 42))]

      it "string name = \"John\";" $ do
        parseAST' "string name = \"John\";"
          `shouldBe` Right [AstDefineVar (Variable "name" String (AstString "John"))]

      it "bool isValid = true;" $ do
        parseAST' "bool isValid = true;"
          `shouldBe` Right [AstDefineVar (Variable "isValid" Bool (AstBool True))]

      it "float pi = 3.14;" $ do
        parseAST' "float pi = 3.14;"
          `shouldBe` Right [AstDefineVar (Variable "pi" Double (AstDouble 3.14))]

      it "char letter = 'A';" $ do
        parseAST' "char letter = 'A';"
          `shouldBe` Right [AstDefineVar (Variable "letter" Char (AstChar 'A'))]

    context "Variables and numbers math" $ do
      it "a + 1" $ do
        parseAST' "a + 1;"
          `shouldBe` Right [AstBinaryFunc "+" (AstVar "a") (AstInt 1)]

      it "a + b" $ do
        parseAST' "a + b;"
          `shouldBe` Right [AstBinaryFunc "+" (AstVar "a") (AstVar "b")]

      it "ab - ba" $ do
        parseAST' "ab - ba;"
          `shouldBe` Right [AstBinaryFunc "-" (AstVar "ab") (AstVar "ba")]

      it "1.45 - ba" $ do
        parseAST' "1.45 - ba;"
          `shouldBe` Right [AstBinaryFunc "-" (AstDouble 1.45) (AstVar "ba")]

      it "a * b" $ do
        parseAST' "a * b;"
          `shouldBe` Right [AstBinaryFunc "*" (AstVar "a") (AstVar "b")]

      it "a * 3.4" $ do
        parseAST' "a * 3.4;"
          `shouldBe` Right [AstBinaryFunc "*" (AstVar "a") (AstDouble 3.4)]

      it "((a ** b) % 30) / 2" $ do
        parseAST' "((a ** b) % 30) / 2;"
          `shouldBe` Right [AstBinaryFunc "/" (AstBinaryFunc "%" (AstBinaryFunc "**" (AstVar "a") (AstVar "b")) (AstInt 30)) (AstInt 2)]

      it "(a + b++) ^ 30" $ do
        parseAST' "(a + b++) ^ 30;"
          `shouldBe` Right [AstBinaryFunc "^" (AstBinaryFunc "+" (AstVar "a") (AstPostfixFunc "++" (AstVar "b"))) (AstInt 30)]

      it "(a + b++) ^ 30" $ do
        parseAST' "(a + b++) ^ 30;"
          `shouldBe` Right [AstBinaryFunc "^" (AstBinaryFunc "+" (AstVar "a") (AstPostfixFunc "++" (AstVar "b"))) (AstInt 30)]

      it "(a + b++) ^ 30" $ do
        parseAST' "(a-- + b++) ^ 30;"
          `shouldBe` Right [AstBinaryFunc "^" (AstBinaryFunc "+" (AstPostfixFunc "--" (AstVar "a")) (AstPostfixFunc "++" (AstVar "b"))) (AstInt 30)]
      it "(a + b++) ^ 30" $ do
        parseAST' "(a + b++) ^ 30;"
          `shouldBe` Right [AstBinaryFunc "^" (AstBinaryFunc "+" (AstVar "a") (AstPostfixFunc "++" (AstVar "b"))) (AstInt 30)]

      it "+a + b++" $ do
        parseAST' "+a + b++;"
          `shouldBe` Right [AstBinaryFunc "+" (AstVar "a") (AstPostfixFunc "++" (AstVar "b"))]

      it "bool a = b == c" $ do
        parseAST' "bool a = b == c;"
          `shouldBe` Right [AstDefineVar (Variable "a" Bool (AstBinaryFunc "==" (AstVar "b") (AstVar "c")))]

      it "bool a = b != c" $ do
        parseAST' "bool a = b != c;"
          `shouldBe` Right [AstDefineVar (Variable "a" Bool (AstBinaryFunc "!=" (AstVar "b") (AstVar "c")))]

      it "b += 1" $ do
        parseAST' "b += 1;"
          `shouldBe` Right [AstBinaryFunc "+=" (AstVar "b") (AstInt 1)]

      it "a -= b" $ do
        parseAST' "a -= b;"
          `shouldBe` Right [AstBinaryFunc "-=" (AstVar "a") (AstVar "b")]

      it "a /= b" $ do
        parseAST' "a /= b;"
          `shouldBe` Right [AstBinaryFunc "/=" (AstVar "a") (AstVar "b")]

      it "a *= b" $ do
        parseAST' "a *= b;"
          `shouldBe` Right [AstBinaryFunc "*=" (AstVar "a") (AstVar "b")]

      it "a **= b" $ do
        parseAST' "a **= b;"
          `shouldBe` Right [AstBinaryFunc "**=" (AstVar "a") (AstVar "b")]

      it "foo[0] = 2" $ do
        parseAST' "foo[0] = 2;"
          `shouldBe` Right [AstBinaryFunc "=" (AstListElem "foo" [0]) (AstInt 2)]

    -- context "Math priorities" $ do

    context "Function declaration" $ do
      it "int add(int a, int b) { return a + b; }" $ do
        parseAST' "int add(int a, int b) { return a + b; }"
          `shouldBe` Right [AstDefineFunc (Function "add" [AstDefineVar (Variable "a" Int AstVoid), AstDefineVar (Variable "b" Int AstVoid)] [AstReturn (AstBinaryFunc "+" (AstVar "a") (AstVar "b"))] Int)]

      it "void printHello() { }" $ do
        parseAST' "void printHello() { }"
          `shouldBe` Right [AstDefineFunc (Function "printHello" [] [] Void)]

      it "bool isEven(int x) { return x % 2 == 0; }" $ do
        parseAST' "bool isEven(int x) { return x % 2 == 0; }"
          `shouldBe` Right [AstDefineFunc (Function "isEven" [AstDefineVar (Variable "x" Int AstVoid)] [AstReturn (AstBinaryFunc "==" (AstBinaryFunc "%" (AstVar "x") (AstInt 2)) (AstInt 0))] Bool)]

      it "char getFirstChar(string s) { return s[0]; }" $ do
        parseAST' "char getFirstChar(string s) { return s[0]; }"
          `shouldBe` Right [AstDefineFunc (Function "getFirstChar" [AstDefineVar (Variable "s" String AstVoid)] [AstReturn (AstListElem "s" [0])] Char)]

      it "void emptyFunction() { }" $ do
        parseAST' "void emptyFunction() { }"
          `shouldBe` Right [AstDefineFunc (Function "emptyFunction" [] [] Void)]

    context "If statement" $ do
      it "if (a < b) { return a; } else { return b; }" $ do
        parseAST' "if (a < b) { return a; } else { return b; }"
          `shouldBe` Right [AstIf (AstBinaryFunc "<" (AstVar "a") (AstVar "b")) (AstBlock [AstReturn (AstVar "a")]) [] (Just (AstBlock [AstReturn (AstVar "b")]))]

      it "if (x == 0) { return 0; }" $ do
        parseAST' "if (x == 0) { return 0; }"
          `shouldBe` Right [AstIf (AstBinaryFunc "==" (AstVar "x") (AstInt 0)) (AstBlock [AstReturn (AstInt 0)]) [] Nothing]

      it "if (y > 10) { return y; } else if (y == 10) { return 0; }" $ do
        parseAST' "if (y > 10) { return y; } else if (y == 10) { return 0; }"
          `shouldBe` Right [AstIf (AstBinaryFunc ">" (AstVar "y") (AstInt 10)) (AstBlock [AstReturn (AstVar "y")]) [AstIf (AstBinaryFunc "==" (AstVar "y") (AstInt 10)) (AstBlock [AstReturn (AstInt 0)]) [] Nothing] Nothing]

      it "if (flag) { x = 1; }" $ do
        parseAST' "if (flag) { x = 1; }"
          `shouldBe` Right [AstIf (AstVar "flag") (AstBlock [AstBinaryFunc "=" (AstVar "x") (AstInt 1)]) [] Nothing]

      it "if (!boolean) { x--; }" $ do
        parseAST' "if (!boolean) { x--; }"
          `shouldBe` Right [AstIf (AstPrefixFunc "!" (AstVar "boolean")) (AstBlock [AstPostfixFunc "--" (AstVar "x")]) [] Nothing]

      it "if (a && b) { return true; } else { return false; }" $ do
        parseAST' "if (a and b) { return true; } else { return false; }"
          `shouldBe` Right [AstIf (AstBinaryFunc "and" (AstVar "a") (AstVar "b")) (AstBlock [AstReturn (AstBool True)]) [] (Just (AstBlock [AstReturn (AstBool False)]))]

      it "if (x > y) { return max; } else if (x < y) { return min; } else { return equal; }" $ do
        parseAST' "if (x > y) { return max; } else if (x < y) { return min; } else { return equal; }"
          `shouldBe` Right [AstIf (AstBinaryFunc ">" (AstVar "x") (AstVar "y")) (AstBlock [AstReturn (AstVar "max")]) [AstIf (AstBinaryFunc "<" (AstVar "x") (AstVar "y")) (AstBlock [AstReturn (AstVar "min")]) [] Nothing] (Just (AstBlock [AstReturn (AstVar "equal")]))]

    context "While loop" $ do
      it "while (i < 10) { i = i + 1; }" $ do
        parseAST' "while (i < 10) { i = i + 1; }"
          `shouldBe` Right [AstLoop Nothing (AstBinaryFunc "<" (AstVar "i") (AstInt 10)) (AstBlock [AstBinaryFunc "=" (AstVar "i") (AstBinaryFunc "+" (AstVar "i") (AstInt 1))])]

      it "while (true) { break; }" $ do
        parseAST' "while (true) { break; }"
          `shouldBe` Right [AstLoop Nothing (AstBool True) (AstBlock [AstBreak])]

      it "while (x != 0) { x = x - 1; }" $ do
        parseAST' "while (x != 0) { x = x - 1; }"
          `shouldBe` Right [AstLoop Nothing (AstBinaryFunc "!=" (AstVar "x") (AstInt 0)) (AstBlock [AstBinaryFunc "=" (AstVar "x") (AstBinaryFunc "-" (AstVar "x") (AstInt 1))])]

      it "while (flag) { continue; }" $ do
        parseAST' "while (flag) { continue; }"
          `shouldBe` Right [AstLoop Nothing (AstVar "flag") (AstBlock [AstContinue])]

      it "while (count < 100) { count += 10; }" $ do
        parseAST' "while (count < 100) { count += 10; }"
          `shouldBe` Right [AstLoop Nothing (AstBinaryFunc "<" (AstVar "count") (AstInt 100)) (AstBlock [AstBinaryFunc "+=" (AstVar "count") (AstInt 10)])]

      it "while (n > 0) { n--; }" $ do
        parseAST' "while (n > 0) { n--; }"
          `shouldBe` Right [AstLoop Nothing (AstBinaryFunc ">" (AstVar "n") (AstInt 0)) (AstBlock [AstPostfixFunc "--" (AstVar "n")])]

      it "while (flag) { x = x * 2; }" $ do
        parseAST' "while (flag) { x = x * 2; }"
          `shouldBe` Right [AstLoop Nothing (AstVar "flag") (AstBlock [AstBinaryFunc "=" (AstVar "x") (AstBinaryFunc "*" (AstVar "x") (AstInt 2))])]

    context "List parsing" $ do
      it "[1, 2, 3]" $ do
        parseAST' "[1, 2, 3];"
          `shouldBe` Right [AstList [AstInt 1, AstInt 2, AstInt 3]]

      it "[true, false, true]" $ do
        parseAST' "[true, false, true];"
          `shouldBe` Right [AstList [AstBool True, AstBool False, AstBool True]]

      it "[\"a\", \"b\"]" $ do
        parseAST' "[\"a\", \"b\"];"
          `shouldBe` Right [AstList [AstString "a", AstString "b"]]

      it "[1, [2, 3], 4]" $ do
        parseAST' "[1, [2, 3], 4];"
          `shouldBe` Right [AstList [AstInt 1, AstList [AstInt 2, AstInt 3], AstInt 4]]

      it "[]" $ do
        parseAST' "[];"
          `shouldBe` Right [AstList []]

    context "Operator parsing" $ do
      it "1 + 2 * 3" $ do
        parseAST' "1 + 2 * 3;"
          `shouldBe` Right [AstBinaryFunc "+" (AstInt 1) (AstBinaryFunc "*" (AstInt 2) (AstInt 3))]

      it "(1 + 2) * 3" $ do
        parseAST' "(1 + 2) * 3;"
          `shouldBe` Right [AstBinaryFunc "*" (AstBinaryFunc "+" (AstInt 1) (AstInt 2)) (AstInt 3)]

      it "1 << 2 | 3" $ do
        parseAST' "1 << 2 | 3;"
          `shouldBe` Right [AstBinaryFunc "|" (AstBinaryFunc "<<" (AstInt 1) (AstInt 2)) (AstInt 3)]

      it "(a and b) or c" $ do
        parseAST' "(a and b) or c;"
          `shouldBe` Right [AstBinaryFunc "or" (AstBinaryFunc "and" (AstVar "a") (AstVar "b")) (AstVar "c")]

    context "Invalid syntax" $ do
      it "int = 42;" $ do
        isLeft (parseAST' "int = 42;") `shouldBe` True

      it "if (x > 0 { return 1; }" $ do
        isLeft (parseAST' "if (x > 0 { return 1; }") `shouldBe` True

      it "while true { x++; }" $ do
        isLeft (parseAST' "while true { x++; }") `shouldBe` True

      it "[1, 2,]" $ do
        isLeft (parseAST' "[1, 2,]") `shouldBe` True

      it "a = b +;" $ do
        isLeft (parseAST' "a = b +;") `shouldBe` True

      it "return; void 42;" $ do
        isLeft (parseAST' "return; void 42;") `shouldBe` True

      it "if (a > b) return max;" $ do
        isLeft (parseAST' "if (a > b) return max;") `shouldBe` True

      it "int 123 = foo;" $ do
        isLeft (parseAST' "int 123 = foo;") `shouldBe` True

      it "bool invalid true;" $ do
        isLeft (parseAST' "bool invalid true;") `shouldBe` True

      it "void func(int x) return x;" $ do
        isLeft (parseAST' "void func(int x) return x;") `shouldBe` True

      it "string greet(string name) { return \"Hello, \" + name; }" $ do
        isLeft (parseAST' "string greet(string name) { return \"Hello, \" + name; }") `shouldBe` True

      it "x = y = 42" $ do
        isLeft (parseAST' "x = y = 42;") `shouldBe` True

      it "x = y = z" $ do
        isLeft (parseAST' "x = y = z;") `shouldBe` True

      it "int x, y, z = 1" $ do
        isLeft (parseAST' "int x, y, z = 1;") `shouldBe` True

      it "ab- - -ba" $ do
        isLeft (parseAST' "ab- - -ba;") `shouldBe` True

      it "if (!bool) { x = 1; }" $ do
        isLeft (parseAST' "if (!bool) { x = 1; }") `shouldBe` True

      it "string = char + float" $ do
        isLeft (parseAST' "string = char + float") `shouldBe` True

      it "true = while" $ do
        isLeft (parseAST' "true = while") `shouldBe` True

      it "false = if" $ do
        isLeft (parseAST' "false = if") `shouldBe` True

      it "if = while" $ do
        isLeft (parseAST' "if = while") `shouldBe` True

      it "while = if" $ do
        isLeft (parseAST' "while = if") `shouldBe` True

      it "[if, while, string, yes, true]" $ do
        isLeft (parseAST' "[if, while, string, yes, true]") `shouldBe` True

      it "a += b /= c *= d" $ do
        isLeft (parseAST' "a += b /= c *= d;") `shouldBe` True
