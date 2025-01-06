{-
-- EPITECH PROJECT, 2024
-- inst [WSL: Ubuntu]
-- File description:
-- interpreter
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module VirtualMachine.Interpreter (
    Numeric (..),
    exec,
    operators,
) where

import Control.Monad.State.Lazy (MonadState (get), evalStateT)
import Data.Int (Int64)
import Debug.Trace (traceShowId)
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

-------------------------
-- operator operations --

-------------------------

numericOp ::
    (Double -> Double -> Double) -> Value -> Value -> Either String Value
numericOp op (N x) (N y) = case fromDouble (op (toDouble x) (toDouble y)) of
    Right n -> Right $ N n
    Left _ -> Right $ D (op (toDouble x) (toDouble y))
numericOp op (N x) (D y) = Right $ D (op (toDouble x) y)
numericOp op (D x) (N y) = Right $ D (op x (toDouble y))
numericOp op (D x) (D y) = Right $ D (op x y)
numericOp _ _ _ = Left "Invalid numeric op"

operatorAdd :: [Value] -> VmState [Value]
operatorAdd (y : x : xs) = eitherS $ (: xs) <$> numericOp (+) x y
operatorAdd _ = fail "expects two number"

operatorSub :: [Value] -> VmState [Value]
operatorSub (y : x : xs) = eitherS $ (: xs) <$> numericOp (-) x y
operatorSub _ = fail "expects two number"

operatorMul :: [Value] -> VmState [Value]
operatorMul (y : x : xs) = eitherS $ (: xs) <$> numericOp (*) x y
operatorMul _ = fail "expects two number"

operatorDiv :: [Value] -> VmState [Value]
operatorDiv (y : x : xs) =
    case y of
        N y' | y' == 0 -> fail "division by zero"
        D y' | y' == 0.0 -> fail "division by zero"
        _ -> eitherS $ (: xs) <$> numericOp (/) x y
operatorDiv _ = fail "expects two number"

operatorMod :: [Value] -> VmState [Value]
operatorMod (N y : N x : xs)
    | y == 0 = fail "modulo by zero"
    | otherwise = return $ N (x `mod` y) : xs
operatorMod _ = fail "xpects two int"

operatorEq :: [Value] -> VmState [Value]
operatorEq (x : y : xs) = return $ B (x == y) : xs
operatorEq _ = fail "expects two value"

operatorNEq :: [Value] -> VmState [Value]
operatorNEq (x : y : xs) = return $ B (x /= y) : xs
operatorNEq _ = fail "expects two value"

operatorLt :: [Value] -> VmState [Value]
operatorLt (y : x : xs) =
    eitherS $
        (: xs)
            <$> case (x, y) of
                (N a, N b) -> Right $ B (toDouble a < toDouble b)
                (N a, D b) -> Right $ B (toDouble a < b)
                (D a, N b) -> Right $ B (a < toDouble b)
                (D a, D b) -> Right $ B (a < b)
                _ -> Left "expects two number"
operatorLt _ = fail "expects two number"

operatorGt :: [Value] -> VmState [Value]
operatorGt (y : x : xs) =
    eitherS $
        (: xs)
            <$> case (x, y) of
                (N a, N b) -> Right $ B (toDouble a > toDouble b)
                (N a, D b) -> Right $ B (toDouble a > b)
                (D a, N b) -> Right $ B (a > toDouble b)
                (D a, D b) -> Right $ B (a > b)
                _ -> Left "expects two number"
operatorGt _ = fail "expects two number"

operatorAnd :: [Value] -> VmState [Value]
operatorAnd (B y : B x : xs) = return $ B (x && y) : xs
operatorAnd _ = fail "And expects two bool"

operatorOr :: [Value] -> VmState [Value]
operatorOr (B y : B x : xs) = return $ B (x || y) : xs
operatorOr _ = fail "Or expects two booleans"

operatorPrint :: [Value] -> VmState [Value]
operatorPrint (S s : xs) = io $ putStr s >> return (N (fromIntegral (length s)) : xs)
operatorPrint (val : xs) =
    io $ (putStr . show) val >> return (N (fromIntegral (length (show val))) : xs)
operatorPrint _ = fail "expects one val"

operatorGet :: [Value] -> VmState [Value]
operatorGet (N idx : L lst : xs)
    | idx >= 0 && idx < fromIntegral (length lst) =
        return $ (lst !! fromIntegral idx) : xs
    | otherwise = fail "Index out of bound"
operatorGet _ = fail "expects a list and an integer index"

operatorSet :: [Value] -> VmState [Value]
operatorSet (val : N idx : L lst : xs)
    | idx >= 0 && idx < fromIntegral (length lst) =
        return $
            L (take (fromIntegral idx) lst ++ [val] ++ drop (fromIntegral idx + 1) lst)
                : xs
    | otherwise = fail "Index out of bound"
operatorSet _ = fail "expects a list, an integer index, and a value"

operators :: [(String, V)]
operators =
    [ ("add", Op operatorAdd),
      ("sub", Op operatorSub),
      ("mul", Op operatorMul),
      ("div", Op operatorDiv),
      ("mod", Op operatorMod),
      ("eq", Op operatorEq),
      ("neq", Op operatorNEq),
      ("less", Op operatorLt),
      ("greater", Op operatorGt),
      ("and", Op operatorAnd),
      ("or", Op operatorOr),
      ("get", Op operatorGet),
      ("set", Op operatorSet),
      ("print", Op operatorPrint)
    ]

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
                        >>= ( \r -> case traceShowId r of
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

-- factCode :: [Instruction]
-- factCode =
--     [ pushArg Nothing 0,
--       push Nothing (N 0),
--       call Nothing "eq",
--       jumpf Nothing (Left 2),
--       push Nothing (N 1),
--       ret Nothing,
--       pushArg Nothing 0,
--       push Nothing (N 1),
--       call Nothing "sub",
--       call Nothing "fact",
--       pushArg Nothing 0,
--       call Nothing "mul",
--       ret Nothing
--     ]

exec :: VmState Value
exec = getNextInstruction >>= exec'
