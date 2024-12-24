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

-- | 'Label' is used for jump purposes it can be empty as not every instructions need one.
type Label = Maybe String

{- | 'Op' reprensets the instructions in the virtual machine.
 It can be used outside of it in order to write your own assembly more easily.
-}
data Op = Op
    { -- | The 'code' if the user wanna use binary instead (is not parsed currently).
      code :: Word8,
      -- | The 'name' is for identifying purposes and writing assembly. It must match the assembly name.
      name :: String,
      -- | The 'inst' is the revelant executed 'Insts' in the virtual machine.
      inst :: Insts,
      -- | A 'Label' in order to be able to access more easlily the different instructions.
      label :: Label
    }
    deriving (Show, Eq)

-- | 'Val' represents the different allowed types in the virtual machine.
data Val
    = N Int64
    | B Bool
    | C Char
    | S String
    | D Double
    | L [Val]
    deriving (Show, Eq)

-- | 'Insts' are the different instructions used in 'Op' (should not be used it is better to use Op)
data Insts
    = Push Val
    | Ret
    | Call
    | JumpF (Either Int64 String)
    | Jump (Either Int64 String)
    | PushArg Int64
    | Noop
    deriving (Show, Eq)

-- | The 'Noop' instruction constructor. 'Noop' means doing nothing.
noop :: Label -> Op
noop = Op 0 "noop" Noop

{- | The 'Call' instuction constructor.
 'Call' is used to invoke an operator specified by the last stack element.
-}
call :: Label -> Op
call = Op 1 "call" Call

{- | The 'Push' instruction constructor.
 'Push' is used to send the value specified as argument to the stack.
-}
push :: Label -> Val -> Op
push l x = Op 2 "push" (Push x) l

{- | The 'Ret' instruction constructor.
 'Ret' is used to retrieve the top of the stack.
-}
ret :: Label -> Op
ret = Op 3 "ret" Ret

{- | The 'JumpF' instruction constructor.
 'JumpF' is used to jump to a specific instruction using a 'Label' or to a relative distance using an 'Int64' if the top of the stack is 'False'.
-}
jumpf :: Label -> Either Int64 String -> Op
jumpf l x = Op 4 "jumpf" (JumpF x) l

{- | The 'Jump' instruction constructor.
 'Jump' is used to jump to a specific instruction using a 'Label' or to a relative distance using an 'Int64'.
-}
jump :: Label -> Either Int64 String -> Op
jump l x = Op 6 "jump" (Jump x) l

{- | The 'PushArg' instruction constructor.
 'PushArg' is used to push to the stack the argument at index specified as an 'Int64'.
-}
pushArg :: Label -> Int64 -> Op
pushArg l x = Op 5 "pushArg" (PushArg x) l
