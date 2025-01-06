{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- State
-}
{-# LANGUAGE InstanceSigs #-}

module VirtualMachine.State (
    initialState,
    io,
    eitherS,
    eitherS',
    register,
    dbg,
    VmState,
    registerL,
    getNextInstruction,
    getPc,
    getStack,
    modifyStack,
    incPc,
    getArgs,
    getMemory,
    getElemInMemory,
    getInArr,
    V (..),
    modifyPc,
    getInstructionIdxAtLabel,
    copyVm,
    dbgStack,
    copyVm',
    appendStack,
) where

import Control.Monad.State (
    MonadIO (..),
    MonadState (get),
    StateT,
    gets,
    modify,
 )
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import VirtualMachine.Instructions (Instruction (Instruction), Value)

data V = V Value | Op ([Value] -> VmState [Value])

instance Show V where
    show :: V -> String
    show (V v) = show v
    show (Op _) = "<operator>"

data Vm = Vm
    { stack :: [Value],
      instructions :: [Instruction],
      memory :: Map String V,
      pc :: Int,
      args :: [Value]
    }
    deriving (Show)

type VmState = StateT Vm IO

initialState :: [Instruction] -> Map String V -> [Value] -> Vm
initialState i m = Vm [] i m 0

copyVm :: [Instruction] -> [Value] -> Vm -> Vm
copyVm i a v = v {stack = [], args = a, instructions = i, pc = 0}

copyVm' :: Int -> [Value] -> Vm -> Vm
copyVm' n a vm = vm {pc = n, stack = [], args = a}

io :: IO a -> VmState a
io = liftIO

eitherS' :: Show e => Either e a -> IO a
eitherS' (Left e') = fail . show $ e'
eitherS' (Right a) = return a

eitherS :: Show e => Either e a -> VmState a
eitherS = io . eitherS'

register' :: (String, V) -> Vm -> Vm
register' (k, nw) v = v {memory = Map.insert k nw $ memory v}

register :: (String, V) -> VmState ()
register i = modify (register' i)

registerL :: [(String, V)] -> VmState ()
registerL = foldr ((>>) . register) (pure ())

dbg :: VmState ()
dbg = get >>= (io . print)

dbgStack :: VmState ()
dbgStack = getStack >>= (io . print)

getInArr :: Int -> [a] -> Maybe a
getInArr 0 (x : _) = Just x
getInArr _ [] = Nothing
getInArr n (_ : xs) = getInArr (n - 1) xs

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

getMemory :: VmState (Map String V)
getMemory = gets memory

getElemInMemory :: String -> VmState (Maybe V)
getElemInMemory s =
    getMemory <&> Map.lookup s

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
