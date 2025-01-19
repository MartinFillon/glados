{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lists
-}
{-# LANGUAGE TupleSections #-}

module Compiler.Translation.ListsStructures (associateTypes, toStructField, translateList) where

import qualified Data.Map as Map
import Memory (Memory, readMemory)
import Parsing.ParserAst (Ast (..), Variable (..))
import VirtualMachine.Instructions (Value (..))

{- | Translates a List of 'Ast' to a List of 'Value'.
 'Value' is the recognised type in VM.
-}
translateList :: [Ast] -> Memory -> [Value]
translateList [] _ = []
translateList (x : xs) mem = maybe [] (\val -> val : translateList xs mem) (associateTypes x mem)

----- Utils

-- | Translates an 'Ast' type in a struct to a tuple (<fieldName>, <fieldValue>).
toStructField :: Memory -> Ast -> Maybe (String, Value)
toStructField mem (AstDefineVar (Variable fieldName _ fieldValue)) =
    fmap (fieldName,) (associateTypes fieldValue mem)
toStructField mem (AstLabel fieldName fieldValue) =
    fmap (fieldName,) (associateTypes fieldValue mem)
toStructField _ _ = Nothing

-- | Associate type 'Ast' to Maybe 'Value'
associateTypes :: Ast -> Memory -> Maybe Value
associateTypes (AstInt n) _ = Just (N (fromIntegral n))
associateTypes (AstBool b) _ = Just (B b)
associateTypes (AstString s) _ = Just (S s)
associateTypes (AstDouble d) _ = Just (D d)
associateTypes (AstChar c) _ = Just (C c)
associateTypes (AstList list) mem = Just (L (translateList list mem))
associateTypes (AstStruct list) mem = Just (
    maybe  -- !! TO DO Check
        (St Map.empty) ( St . Map.fromList)
        (mapM (toStructField mem) list)
    )
associateTypes (AstListElem var _) mem = case readMemory mem var of -- !! TODO Check
    Just (AstList (x : _)) -> associateTypes x mem
    _ -> Nothing
associateTypes (AstArg ast _) mem = associateTypes ast mem
associateTypes (AstVar var) mem =
    (`associateTypes` mem) =<< readMemory mem var
associateTypes _ _ = Nothing
