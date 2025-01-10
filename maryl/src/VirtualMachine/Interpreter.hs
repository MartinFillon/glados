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
    getElemInMemory s
        >>= ( \e -> case e of
                Just (Op f) -> getStack >>= f >>= modifyStack
                Just ((V (Bi f))) ->
                    (copyVm f <$> getStack <*> get >>= (io . evalStateT exec))
                        >>= appendStack
                Nothing ->
                    getInstructionIdxAtLabel s
                        >>= ( \r -> case r of
                                Just idx ->
                                    (copyVm' idx <$> getStack <*> get >>= (io . evalStateT exec))
                                        >>= appendStack
                                Nothing -> fail $ "could not find an element with label: " ++ s
                            )
                (Just t) -> fail $ "call unimplemented for " ++ show t
            )

execJumpF' :: Either Int String -> Value -> VmState ()
execJumpF' jd (B False) = execJump jd
execJumpF' _ _ = pure ()

execJumpF :: Either Int String -> VmState ()
execJumpF jd = getStack >>= execJumpF' jd . head

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
                Just (V v') -> getStack >>= modifyStack . (v' :) >> return ()
                Just _ -> fail "cannot access an operator using get"
            )

execInstruction :: Instruction -> VmState (Maybe Value)
execInstruction (Instruction _ _ Ret _) =
    getStack >>= eitherS . execRet
execInstruction (Instruction _ _ (Push x) _) =
    getStack >>= modifyStack . (x :) >> return Nothing
execInstruction (Instruction _ _ Noop _) = return Nothing
execInstruction (Instruction _ _ (PushArg x) _) = execPushArg x >> return Nothing
execInstruction (Instruction _ _ (Call n) _) = execCall n >> return Nothing
execInstruction (Instruction _ _ (Jump j) _) = execJump j >> return Nothing
execInstruction (Instruction _ _ (JumpIfFalse j) _) = execJumpF j >> return Nothing
execInstruction (Instruction _ _ (Load n v) _) = register (n, V v) >> return Nothing
execInstruction (Instruction _ _ (Get n) _) = execGet n >> return Nothing
execInstruction i = fail $ "Not handled" ++ name i

exec' :: Maybe Instruction -> VmState Value
exec' (Just i) =
    execInstruction i
        >>= maybe (incPc >> getNextInstruction >>= exec') return
exec' Nothing = return $ N 0

exec :: VmState Value
exec = getNextInstruction >>= exec'
