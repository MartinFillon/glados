{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}

module VirtualMachine.Instructions (
    Insts (..),
    Val (..),
    Op (..),
    push,
    call,
    ret,
    noop,
    pushArg,
    jumpf,
    jump,
    Label,
) where

import Data.Int (Int64)
import Data.Word (Word8)

type Label = Maybe String

data Op = Op
    { code :: Word8,
      name :: String,
      inst :: Insts,
      label :: Label
    }
    deriving (Show, Eq)

data Val
    = N Int64
    | B Bool
    | C Char
    | S String
    | D Double
    | L [Val]
    deriving (Show, Eq)

data Insts
    = Push Val
    | Ret
    | Call
    | JumpF (Either Int64 String)
    | Jump (Either Int64 String)
    | PushArg Int64
    | Noop
    deriving (Show, Eq)

noop :: Label -> Op
noop = Op 0 "Noop" Noop

call :: Label -> Op
call = Op 1 "Call" Call

push :: Label -> Val -> Op
push l x = Op 2 "Push" (Push x) l

ret :: Label -> Op
ret = Op 3 "Ret" Ret

jumpf :: Label -> Either Int64 String -> Op
jumpf l x = Op 4 "JumpF" (JumpF x) l

jump :: Label -> Either Int64 String -> Op
jump l x = Op 6 "Jump" (Jump x) l

pushArg :: Label -> Int64 -> Op
pushArg l x = Op 5 "PushArg" (PushArg x) l
