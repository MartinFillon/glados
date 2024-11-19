{-# LANGUAGE InstanceSigs #-}

module Printer (Paint (..), Style (..), Color (..), reset) where

data Color
    = Red
    | Green
    | Blue
    | Yellow
    | Magenta
    | Cyan
    | White
    deriving (Enum)

instance Show Color where
    show :: Color -> String
    show Red = "\x1b[31m"
    show Green = "\x1b[32m"
    show Blue = "\x1b[34m"
    show Yellow = "\x1b[33m"
    show Magenta = "\x1b[35m"
    show Cyan = "\x1b[36m"
    show White = "\x1b[37m"

reset :: String
reset = "\x1b[0m"

data Style
    = Bold
    | Underlined
    deriving (Enum)

instance Show Style where
    show :: Style -> String
    show Bold = "\x1b[1m"
    show Underlined = "\x1b[4m"

class Paint a where
    paint :: Bool -> a -> String
