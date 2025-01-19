{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- MemorySpec
-}

module MemorySpec (spec) where

import qualified Data.Map as Map
import Memory
import Parsing.ParserAst (Ast (..), Function (..), MarylType (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Memory module" $ do
        it "initializes an empty memory" $ do
            initMemory `shouldBe` Map.empty

        it "adds a variable to memory" $ do
            let mem = initMemory
                result = addMemory mem "var" (AstString "value")
            result `shouldBe` Right (Map.fromList [("var", AstString "value")])

        it "prevents adding a duplicate variable to memory" $ do
            let mem = Map.fromList [("var", AstString "value")]
                result = addMemory mem "var" (AstString "newValue")
            result `shouldBe` Left "multiple definition of \"var\""

        it "updates an existing variable in memory" $ do
            let mem = Map.fromList [("var", AstString "value")]
                updatedMem = updateMemory mem "var" (AstString "newValue")
            updatedMem `shouldBe` Map.fromList [("var", AstString "newValue")]

        it "reads an existing variable from memory" $ do
            let mem = Map.fromList [("var", AstString "value")]
            readMemory mem "var" `shouldBe` Just (AstString "value")

        it "returns Nothing when reading a non-existent variable" $ do
            let mem = Map.fromList [("var", AstString "value")]
            readMemory mem "missingVar" `shouldBe` Nothing

        it "generates a unique loop name" $ do
            let mem = Map.fromList [("loop1", AstString "value"), ("loop2", AstString "value")]
            generateUniqueLoopName mem `shouldBe` "loop3"

        it "generates a unique else name" $ do
            let mem = Map.fromList [("else1", AstString "value"), ("else3", AstString "value")]
            generateUniqueElseName mem `shouldBe` "else4"

        it "frees memory by filtering out non-keepable Ast types" $ do
            let mem =
                    Map.fromList
                        [ ("var1", AstDefineFunc (Function "f" [] [] Void)),
                          ("var2", AstString "value"),
                          ("var4", AstIf (AstString "cond") (AstString "then") [] Nothing)
                        ]
                freedMem = freeMemory mem
            freedMem
                `shouldBe` Map.fromList
                    [ ("var1", AstDefineFunc (Function "f" [] [] Void)),
                      ("var4", AstIf (AstString "cond") (AstString "then") [] Nothing)
                    ]
