{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Numeric
-}
{-# LANGUAGE InstanceSigs #-}

module Numeric (Numeric (..)) where

import Data.Int (Int64)

class Numeric a where
    toDouble :: a -> Double
    fromDouble :: Double -> Either String a

instance Numeric Int64 where
    toDouble :: Int64 -> Double
    toDouble = fromIntegral
    fromDouble :: Double -> Either String Int64
    fromDouble d =
        if (fromIntegral (round d :: Int64) :: Double) == d
            then Right (round d :: Int64)
            else Left "Cant convert to Int64 without loss"

instance Numeric Double where
    toDouble :: Double -> Double
    toDouble = id
    fromDouble :: Double -> Either String Double
    fromDouble = Right
