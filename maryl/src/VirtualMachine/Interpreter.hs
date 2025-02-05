{-# LANGUAGE TupleSections #-}
{-
-- EPITECH PROJECT, 2024
-- inst [WSL: Ubuntu]
-- File description:
-- interpreter
-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module VirtualMachine.Interpreter (
    exec,
) where

import Control.Monad.State.Lazy (MonadState (get), evalStateT)
import VirtualMachine.Instructions (
    Inst (..),
    Instruction (..),
    Value (..),
 )
import VirtualMachine.State (
    V (..),
    VmState,
    appendStack,
    copyVm,
    copyVm',
    eitherS,
    getArgs,
    getElemInMemory,
    getInArr,
    getInstructionIdxAtLabel,
    getNextInstruction,
    getOperator,
    getStack,
    incPc,
    io,
    modifyPc,
    modifyStack,
    register,
 )

execRet :: [Value] -> Either String (Maybe Value)
execRet [] = Left "No values on stack"
execRet (x : _) = Right $ Just x

execPushArg' :: Int -> [Value] -> Maybe Value -> VmState ()
execPushArg' n _ Nothing = fail $ "No values at the index " ++ show n ++ " arg"
execPushArg' _ st (Just x) = modifyStack (x : st)

execPushArg :: Int -> VmState ()
execPushArg n = getArgs >>= (\r -> getStack >>= (\s -> execPushArg' n s r)) . getInArr n

execCall :: String -> VmState ()
execCall s =
    getOperator s
        >>= ( \e -> case e of
                Just (Op f) -> getStack >>= f >>= modifyStack
                Just ((V (Bi f))) ->
                    (copyVm f <$> getStack <*> get >>= (io . evalStateT exec))
                        >>= appendStack
                Nothing ->
                    getInstructionIdxAtLabel s
                        >>= ( \r -> case r of
                                Just idx ->
                                    (copyVm' idx <$> (reverse <$> getStack) <*> get >>= (io . evalStateT exec))
                                        >>= (\v -> modifyStack [v])
                                Nothing -> fail $ "could not find an element with label: " ++ s
                            )
                (Just t) -> fail $ "call unimplemented for " ++ show t
            )

execJumpF' :: Either Int String -> Value -> VmState ()
execJumpF' jd (B False) = execJump jd
execJumpF' _ _ = pure ()

dropAndGet' :: [Value] -> VmState Value
dropAndGet' (x : xs) = modifyStack xs >> return x
dropAndGet' [] = fail "empty stack"

dropAndGet :: VmState Value
dropAndGet = getStack >>= dropAndGet'

execJumpF :: Either Int String -> VmState ()
execJumpF jd = dropAndGet >>= execJumpF' jd

execJump :: Either Int String -> VmState ()
execJump (Left n)
    | n < 0 = modifyPc (+ (n - 1))
    | otherwise = modifyPc (+ n)
execJump (Right lbl) =
    getInstructionIdxAtLabel lbl
        >>= ( \r -> case r of
                Just idx -> modifyPc (const (idx - 1))
                Nothing -> fail $ "could not find an element with label: " ++ lbl
            )

execGet :: String -> VmState ()
execGet n =
    getElemInMemory n
        >>= ( \v -> case v of
                Nothing -> fail $ "could not find constant " ++ n
                Just v' -> getStack >>= modifyStack . (v' :) >> return ()
            )

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs

dup1 :: [a] -> [a]
dup1 (x : xs) = x : x : xs
dup1 [] = []

execLoad :: String -> VmState ()
execLoad n = dropAndGet >>= register . (n,)

execInstruction :: Instruction -> VmState (Maybe Value)
execInstruction (Instruction _ _ Ret _) =
    getStack >>= eitherS . execRet
execInstruction (Instruction _ _ (Push x) _) =
    getStack >>= modifyStack . (x :) >> return Nothing
execInstruction (Instruction _ _ Void _) = getStack >>= (modifyStack . drop1) >> return Nothing
execInstruction (Instruction _ _ Dup _) = getStack >>= (modifyStack . dup1) >> return Nothing
execInstruction (Instruction _ _ Noop _) = return Nothing
execInstruction (Instruction _ _ (PushArg x) _) = execPushArg x >> return Nothing
execInstruction (Instruction _ _ (Call n) _) = execCall n >> return Nothing
execInstruction (Instruction _ _ (Jump j) _) = execJump j >> return Nothing
execInstruction (Instruction _ _ (JumpIfFalse j) _) = execJumpF j >> return Nothing
execInstruction (Instruction _ _ (Load n) _) = execLoad n >> return Nothing
execInstruction (Instruction _ _ (Get n) _) = execGet n >> return Nothing
execInstruction i = fail $ "Not handled" ++ name i

-- dbg :: VmState ()
-- dbg = get >>= (io . printVM)

-- printVM :: Vm -> IO ()
-- printVM (Vm s i m p a _) =
--     putStrLn "==============================\nStack :"
--         >> print s
--         >> putStrLn "==============================\nArgs :"
--         >> print a
--         >> putStrLn "==============================\nHandles :"
--         >> mapM print (handles m)
--         >> putStrLn "\n\nInsts :"
--         >> mapM print i
--         >> putStrLn "\nCurrent: "
--         >> print (i !! p)
--         >> void getLine

exec' :: Maybe Instruction -> VmState Value
exec' (Just i) =
    -- dbg >>
    execInstruction i
        >>= maybe (incPc >> getNextInstruction >>= exec') return
exec' Nothing = return $ N 84

exec :: VmState Value
exec = getNextInstruction >>= exec'
