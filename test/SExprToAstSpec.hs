{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- SExprToAstSpec
-}

module SExprToAstSpec (spec) where

import Data.Maybe (fromJust)
import HelperSpec (shouldBeApproximatelyAst)
import Parsing.ParserSExpr (Atom (..), Sexpr (..))
import Parsing.SExprToAst (Ast (..), Function (..), sexprToAST)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "SExprToAst" $ do
        it "should return a valid AST number" $ do
            sexprToAST (Atom (Number 42)) `shouldBe` Just (AstInt 42)
        it "should return a valid AST float" $ do
            fromJust (sexprToAST (Atom (Float 42.42)))
                `shouldBeApproximatelyAst` AstFloat 42.42
        it "should return a valid AST bool" $ do
            sexprToAST (Atom (Bool True)) `shouldBe` Just (AstBool True)
        it "should return a valid AST symbol" $ do
            sexprToAST (Atom (String "test")) `shouldBe` Just (AstSymbol "test" AstVoid)
        it "should return a valid AST define" $ do
            sexprToAST
                (List [Atom (String "define"), Atom (String "test"), Atom (Number 42)])
                `shouldBe` Just (Define "test" (AstInt 42))
        it "should return a valid AST call" $ do
            sexprToAST (List [Atom (String "test"), Atom (Number 42)])
                `shouldBe` Just (Call (Function "test" [AstInt 42]))
        it "should return a valid AST condition" $ do
            sexprToAST
                ( List
                    [ Atom (String "if"),
                      Atom (Bool True),
                      Atom (Number 42),
                      Atom (Number 42)
                    ]
                )
                `shouldBe` Just
                    ( Condition
                        (Function "if" [AstBool True, AstInt 42, AstInt 42])
                    )
        it "should define a function" $ do
            sexprToAST
                ( List
                    [ Atom (String "define"),
                      List [Atom (String "test"), Atom (String "a")],
                      List [Atom (String "+"), Atom (Number 42), Atom (String "a")]
                    ]
                )
                `shouldBe` Just
                    ( Define
                        "test"
                        (Lambda ["a"] (Call (Function "+" [AstInt 42, AstSymbol "a" AstVoid])))
                    )
        it "should define a lambda" $ do
            sexprToAST
                ( List
                    [ Atom (String "lambda"),
                      List [Atom (String "a")],
                      List [Atom (String "+"), Atom (Number 42), Atom (String "a")]
                    ]
                )
                `shouldBe` Just (Lambda ["a"] (Call (Function "+" [AstInt 42, AstSymbol "a" AstVoid])))
        it "should do an apply" $ do
            sexprToAST
                ( List
                    [ Atom (Number 42),
                      Atom (Number 42)
                    ]
                )
                `shouldBe` Just (Apply (AstInt 42) [AstInt 42])
        it "should fail on empty list" $ do
            sexprToAST (List []) `shouldBe` Nothing
