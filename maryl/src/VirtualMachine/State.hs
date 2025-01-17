{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- State
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}

{- |
Description : This is the type used in the virtual machine in order for it to keep it state.
-}
module VirtualMachine.State (
    -- * Types
    VmState,
    V (..),
    Vm (..),

    -- * Functions

    -- The functions are easy ways to interface with the 'Vm'.
    initialState,
    io,
    eitherS,
    register,
    dbg,
    registerL,
    getNextInstruction,
    getPc,
    getStack,
    handles,
    modifyStack,
    incPc,
    getArgs,
    getMemory,
    getElemInMemory,
    getInArr,
    modifyPc,
    getInstructionIdxAtLabel,
    copyVm,
    dbgStack,
    copyVm',
    appendStack,
    getHandleInMemory,
    registerHandle,
    initialMemory,
    ioCatch,
    setError,
    getError,
    getOperator,
) where

import Control.Exception (catch)
import Control.Monad.State (
    MonadIO (..),
    MonadState (get),
    StateT,
    gets,
    modify,
 )
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.IO.Exception (IOException)
import GHC.IO.Handle (Handle)
import System.IO (stderr, stdin, stdout)
import VirtualMachine.Instructions (Instruction (Instruction), Value)

{- | The 'V' data is just a wrapper between 'Value' and Operators 'Op'.
 Operators are a special type of value that takes a function as parameter.
-}
data V = V Value | Op ([Value] -> VmState [Value])

instance Show V where
    show :: V -> String
    show (V v) = show v
    show (Op _) = "<operator>"

data VmMemory = VmMemory
    { values :: Map String V,
      vars :: Map String Value,
      handles :: [Handle]
    }
    deriving (Show)

initialMemory :: Map String V -> VmMemory
initialMemory op = VmMemory op Map.empty [stdin, stdout, stderr]

registerValue :: VmMemory -> (String, Value) -> VmMemory
registerValue v@(VmMemory _ vls _) (k, k') = v {vars = Map.insert k k' vls}

copyMemory :: VmMemory -> VmMemory
copyMemory v = v {vars = Map.empty}

{- | The 'Vm' Data is what holds every useful information about the state of the vitual machine.
It can be used by itself but it is highly recommended to use it with 'VmState'.
-}
data Vm = Vm
    { -- | The 'stack' field, as its name implies, is a stack of values used to pass arguments to the different functions.
      stack :: [Value],
      -- | The 'instructions' field, is the whole list of instructions to be executed by the program, it is composed of 'Instruction'.
      instructions :: [Instruction],
      -- | The 'memory' field, as it name implies is where the operators,
      --       and constants values are stored. It is, for the moment,
      --       only used for operators or testing purposes.
      memory :: VmMemory,
      -- | The 'pc' field, also known as program counter, is the current index executed in the list of instructions specified in the field 'instructions.
      pc :: Int,
      -- | The 'args' field, is the arguments passed to the program at startup.
      args :: [Value],
      -- | The 'error' field, is used to specify that an error occured during the last syscall it can be access with the operator error,
      error' :: Bool
    }
    deriving (Show)

{- | The 'VmState' type alias is constituated of the 'StateT' monad, the 'Vm' and the 'IO' monad.
It is used in order to be able to easily use the 'MonadState' class.
This type permit us to have a state which is of type 'Vm'. While being able to run 'IO' functions such as 'print' or 'putStr'.
This type is the backbone of our Virtual Machine.
-}
type VmState = StateT Vm IO

{- | The 'initialState' function describes the recommended way to create a new 'Vm' in order to be plugged into 'VmState'.
It takes a list of 'Instruction', followed by a map representing the initial 'memory' and lastly a list of 'Value' for arguments.
This function will always initialise an empty 'stack' and a 'pc' at 0

>>> initialState [push 10, ret 5] (Map.fromList []) []
-}
initialState :: [Instruction] -> VmMemory -> [Value] -> Vm
initialState i m a = Vm [] i m 0 a False

{- | The 'copyVm' function is used to copy a vm state and its memory and change its instructions.
It also clears the stack and takes the old one as argument in order for it to become the new args.
-}
copyVm :: [Instruction] -> [Value] -> Vm -> Vm
copyVm i a v = v {stack = [], args = a, instructions = i, pc = 0, memory = copyMemory (memory v)}

{- | The 'copyVm'' function is used to copy a vm state and its memory and change its pc.
It also clears the stack and takes the old one as argument in order for it to become the new args.
-}
copyVm' :: Int -> [Value] -> Vm -> Vm
copyVm' n a vm = vm {pc = n, stack = [], args = a}

{- | The 'io' function is a wrapper for 'liftIO'. It is mainly used to run io function such as 'print' or 'putStr'.

Here is an example of a print string from the stack.

>>> operatorPrint (S s : xs) = io $ putStr s
-}
io :: IO a -> VmState a
io = liftIO

setError :: Bool -> VmState ()
setError e = modify (\v -> v {error' = e})

getError :: VmState Bool
getError = gets error'

ioCatch :: IO a -> b -> VmState (Either a b)
ioCatch v b = io (catch (v <&> Left) (\e -> (const $ return $ Right b) (e :: IOException)))

eitherS' :: Show e => Either e a -> IO a
eitherS' (Left e') = fail . show $ e'
eitherS' (Right a) = return a

{- | The 'eitherS' function, serves as a lift for the 'Either' monad.
It can be used like 'io' but for functions returning a 'Either'.
-}
eitherS :: Show e => Either e a -> VmState a
eitherS = io . eitherS'

register' :: (String, Value) -> Vm -> Vm
register' k v = v {memory = registerValue (memory v) k}

register :: (String, Value) -> VmState ()
register (n, v) = modify (register' (n, v))

registerL :: [(String, Value)] -> VmState ()
registerL = foldr ((>>) . register) (pure ())

registerHandle' :: Handle -> VmMemory -> VmState VmMemory
registerHandle' h vmm@(VmMemory _ _ h') = modify (\v -> v {memory = vmm {handles = h' ++ [h]}}) >> getMemory

registerHandle :: Handle -> VmState Int64
registerHandle h =
    getMemory
        >>= registerHandle' h
        >>= ( \v -> case elemIndex h (handles v) of
                Just n -> pure $ fromIntegral n
                _ -> fail "should not happen"
            )

{- | The 'dbg' function is used for, as it name tells, debug purposes.
It will do a basic print of the state and won't modify it.
It is a safe function.
-}
dbg :: VmState ()
dbg = get >>= (io . printVM)

printVM :: Vm -> IO ()
printVM (Vm s i m p _ _) =
    putStrLn "==============================\nStack :"
        >> print s
        >> putStrLn "==============================\nHandles :"
        >> mapM print (handles m)
        >> putStrLn "\n\nInsts :"
        >> mapM print i
        >> putStrLn "\nCurrent: "
        >> print (i !! p)

{- | The 'dgbStack' function is used for, as it name tells, debug purposes.
It will do a basic print of the stack and won't modify it.
It is a safe function.
-}
dbgStack :: VmState ()
dbgStack = getStack >>= (io . print)

-- | The 'getInArr' function is used to retrive an element of an Array.
getInArr :: Int -> [a] -> Maybe a
getInArr 0 (x : _) = Just x
getInArr _ [] = Nothing
getInArr n (_ : xs) = getInArr (n - 1) xs

-- | The 'getNextInstruction' function is used to retrive the next Instruction.
getNextInstruction :: VmState (Maybe Instruction)
getNextInstruction = getInArr <$> getPc <*> gets instructions

getPc :: VmState Int
getPc = gets pc

incPc :: VmState ()
incPc = modifyPc (+ 1)

modifyPc :: (Int -> Int) -> VmState ()
modifyPc f = modify (\v -> v {pc = f $ pc v})

getStack :: VmState [Value]
getStack = gets stack

getArgs :: VmState [Value]
getArgs = gets args

getMemory :: VmState VmMemory
getMemory = gets memory

getElemInMemory :: String -> VmState (Maybe Value)
getElemInMemory s =
    getMemory <&> Map.lookup s . vars

getOperator :: String -> VmState (Maybe V)
getOperator s = getMemory <&> Map.lookup s . values

getHandleInMemory :: Int64 -> VmState Handle
getHandleInMemory i =
    getMemory <&> (!! fromIntegral i) . handles

modifyStack :: [Value] -> VmState ()
modifyStack vs = modify (\v -> v {stack = vs})

findInstructionWithLabel' :: String -> Instruction -> Bool
findInstructionWithLabel' s (Instruction _ _ _ (Just l)) = l == s
findInstructionWithLabel' _ _ = False

findInstructionWithLabel :: String -> [Instruction] -> Maybe Instruction
findInstructionWithLabel s l = case filter (findInstructionWithLabel' s) l of
    [] -> Nothing
    xs -> Just $ head xs

getInstructionIdxAtLabel :: String -> VmState (Maybe Int)
getInstructionIdxAtLabel s =
    gets instructions
        >>= (\l -> return (findInstructionWithLabel s l >>= (`elemIndex` l)))

appendStack :: Value -> VmState ()
appendStack v = getStack >>= (\s -> modifyStack (v : s))
