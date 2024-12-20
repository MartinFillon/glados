{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Bytes
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module VirtualMachine.Bytes (ToBytes (..), stringToBytes) where

import Data.Bits (Bits (..))
import Data.Char (ord)
import Data.Int (Int64)
import Data.Word (Word8)

class ToBytes a where
    toByte :: a -> [Word8]

instance ToBytes Int64 where
    toByte :: Int64 -> [Word8]
    toByte n =
        [ getNByte n 56,
          getNByte n 48,
          getNByte n 40,
          getNByte n 32,
          getNByte n 24,
          getNByte n 16,
          getNByte n 8,
          fromIntegral n .&. 0xFF
        ]

instance ToBytes Int where
    toByte :: Int -> [Word8]
    toByte n =
        [ getNByte n 24,
          getNByte n 16,
          getNByte n 8,
          fromIntegral n .&. 0xFF
        ]

stringToBytes :: String -> [Word8]
stringToBytes = map (fromIntegral . ord)

getNByte :: (Integral a, Bits a) => a -> Int -> Word8
getNByte n k = fromIntegral (shiftR n k .&. 0xFF)
