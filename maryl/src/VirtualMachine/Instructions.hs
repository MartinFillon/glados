{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Instructions
-}
{-# LANGUAGE InstanceSigs #-}

module VirtualMachine.Instructions (
    Inst (..),
    Value (..),
    Instruction (..),
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

{- | 'Instruction' reprensets the instructions in the virtual machine.
 It can be used outside of it in order to write your own assembly more easily.
-}
data Instruction = Instruction
    { -- | The 'code' if the user wanna use binary instead (is not parsed currently).
      code :: Word8,
      -- | The 'name' is for identifying purposes and writing assembly. It must match the assembly name.
      name :: String,
      -- | The 'inst' is the revelant executed 'Inst' in the virtual machine.
      inst :: Inst,
      -- | A 'Label' in order to be able to access more easlily the different instructions.
      label :: Label
    }
    deriving (Show, Eq)

-- | 'Value' represents the different allowed types in the virtual machine.
data Value
    = N Int64
    | B Bool
    | S String
    | L [Value]
    | D Double
    | Bi [Inst]

instance Show Value where
    show :: Value -> String
    show (N n) = show n
    show (B b) = show b
    show (S s) = show s
    show (L vs) = show vs
    show (D d) = show d
    show (Bi _) = "<builtin>"

instance Eq Value where
    (==) :: Value -> Value -> Bool
    (N a) == (N b) = a == b
    (B a) == (B b) = a == b
    (S a) == (S b) = a == b
    (L a) == (L b) = a == b
    (D a) == (D b) = a == b
    (Bi a) == (Bi b) = a == b
    _ == _ = False

-- | 'Inst' are the different instructions used in 'Instruction' (should not be used it is better to use Instruction)
data Inst
    = Noop
    | Push Value
    | PushArg Int
    | Call String
    | Ret
    | JumpIfFalse (Either Int String)
    | Jump (Either Int String)
    deriving (Show, Eq)

-- | The 'Noop' instruction constructor. 'Noop' means doing nothing.
noop :: Label -> Instruction
noop = Instruction 0 "noop" Noop

{- | The 'Call' instuction constructor.
 'Call' is used to invoke an operator specified by argument.
-}
call :: Label -> String -> Instruction
call l s = Instruction 1 "call" (Call s) l

{- | The 'Push' instruction constructor.
 'Push' is used to send the value specified as argument to the stack.
-}
push :: Label -> Value -> Instruction
push l x = Instruction 2 "push" (Push x) l

{- | The 'Ret' instruction constructor.
 'Ret' is used to retrieve the top of the stack.
-}
ret :: Label -> Instruction
ret = Instruction 3 "ret" Ret

{- | The 'JumpF' instruction constructor.
 'JumpF' is used to jump to a specific instruction using a 'Label' or to a relative distance using an 'Int64' if the top of the stack is 'False'.
-}
jumpf :: Label -> Either Int String -> Instruction
jumpf l x = Instruction 4 "jumpf" (JumpIfFalse x) l

{- | The 'Jump' instruction constructor.
 'Jump' is used to jump to a specific instruction using a 'Label' or to a relative distance using an 'Int'.
-}
jump :: Label -> Either Int String -> Instruction
jump l x = Instruction 6 "jump" (Jump x) l

{- | The 'PushArg' instruction constructor.
 'PushArg' is used to push to the stack the argument at index specified as an 'Int'.
-}
pushArg :: Label -> Int -> Instruction
pushArg l x = Instruction 5 "pushArg" (PushArg x) l
