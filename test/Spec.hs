{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Spec
-}

import Test.Hspec
import qualified Main

main :: IO ()
main = hspec $ do
    describe "evalAST" $ do
        it "evaluates literal integers" $ do
            Main.evalAST (Main.AstInt 42) `shouldBe` Just (Main.AstInt 42)
        
        it "evaluates Boolean true" $ do
            Main.evalAST (Main.AstBool True) `shouldBe` Just (Main.AstBool True)
        
        it "evaluates addition" $ do
            Main.evalAST (Main.Call (Main.Function "+" [Main.AstInt 1, Main.AstInt 2]))
                `shouldBe` Just (Main.AstInt 3)
        
        it "evaluates multiplication" $ do
            Main.evalAST (Main.Call (Main.Function "*" [Main.AstInt 3, Main.AstInt 4]))
                `shouldBe` Just (Main.AstInt 12)
        
        it "evaluates Boolean AND" $ do
            Main.evalAST (Main.Call (Main.Function "and" [Main.AstBool True, Main.AstBool False]))
                `shouldBe` Just (Main.AstBool False)
        
        it "evaluates lambda call" $ do
            let lambdaExpr = Main.Lambda ["x", "y"] (Main.Call (Main.Function "/" [Main.AstSymbol "x" Nothing, Main.AstSymbol "y" Nothing]))
            Main.evalAST (Main.Apply lambdaExpr [Main.AstInt 10, Main.AstInt 2])
                `shouldBe` Just (Main.AstInt 5)
        
        it "evaluates define" $ do
            let defineExpr = Main.Define "x" (Main.AstInt 10)
            let symbolX = Main.evalAST defineExpr
            Main.evalAST (Main.Call (Main.Function "+" [Main.AstSymbol "x" symbolX, Main.AstInt 5]))
                `shouldBe` Just (Main.AstInt 15)
        
        it "evaluates modulo operation" $ do
            Main.evalAST (Main.Call (Main.Function "%" [Main.AstInt 10, Main.AstInt 3]))
                `shouldBe` Just (Main.AstInt 1)
        
        it "evaluates less than" $ do
            Main.evalAST (Main.Call (Main.Function "<" [Main.AstInt 3, Main.AstInt 5]))
                `shouldBe` Just (Main.AstBool True)
        
        it "evaluates equals" $ do
            Main.evalAST (Main.Call (Main.Function "eq?" [Main.AstInt 4, Main.AstInt 4]))
                `shouldBe` Just (Main.AstBool True)
        
        it "evaluates greater than" $ do
            Main.evalAST (Main.Call (Main.Function ">" [Main.AstInt 7, Main.AstInt 2]))
                `shouldBe` Just (Main.AstBool True)