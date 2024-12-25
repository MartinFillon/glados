{-
-- EPITECH PROJECT, 2024
-- inst [WSL: Ubuntu]
-- File description:
-- interpreter
-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module VirtualMachine.Interpreter (
    Numeric (..),
    exec,
) where

import Data.Int (Int64)
import VirtualMachine.Instructions (Inst (..), Instruction (Instruction), Value (..))
import VirtualMachine.State (
    V (..),
    VmState,
    eitherS,
    getArgs,
    getElemInMemory,
    getInArr,
    getNextInstruction,
    getStack,
    incPc,
    io,
    modifyStack,
    registerL,
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
operatorPrint (val : xs) = io $ (putStr . show) val >> return (N (fromIntegral (length (show val))) : xs)
operatorPrint _ = fail "expects one val"

operatorGet :: [Value] -> VmState [Value]
operatorGet (N idx : L lst : xs)
    | idx >= 0 && idx < fromIntegral (length lst) = return $ (lst !! fromIntegral idx) : xs
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
      ("less", Op operatorLt),
      ("greater", Op operatorGt),
      ("and", Op operatorAnd),
      ("or", Op operatorOr),
      ("get", Op operatorGet),
      ("set", Op operatorSet),
      ("print", Op operatorPrint)
    ]

-- ioOperators :: Map String (Stack -> IO (Either String Value))
-- ioOperators = Map.fromList [("print", operatorPrint)]

-- execCall' :: Vm -> Either Value (Stack -> IO (Either String Value)) -> IO (Either String Vm)
-- execCall' v (Left (Op f)) = case f (stack v) of
--     Right val -> return $ Right v {stack = val : drop 2 (stack v)}
--     Left e -> return $ Left e
-- execCall' v (Right f) =
--     f (stack v)
--         >>= ( \r -> case r of
--                 Right val -> return $ Right v {stack = val : drop 1 (stack v)}
--                 Left e -> return $ Left e
--             )
-- execCall' _ _ = return $ Left "Function not found"

-- execCall :: Vm -> String -> IO (Either String Vm)
-- execCall v@(Vm _ _ _ m _) s = case getFromMem m s of
--     Just f -> execCall' v f

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
                Op f -> getStack >>= f >>= modifyStack
                _ -> fail "unimplemented"
            )

execInstruction :: Instruction -> VmState (Maybe Value)
execInstruction (Instruction _ _ Ret _) =
    getStack >>= eitherS . execRet
execInstruction (Instruction _ _ (Push x) _) =
    getStack >>= modifyStack . (x :) >> return Nothing
execInstruction (Instruction _ _ Noop _) = return Nothing
execInstruction (Instruction _ _ (PushArg x) _) = execPushArg x >> return Nothing
execInstruction (Instruction _ _ (Call n) _) = execCall n >> return Nothing
execInstruction _ = eitherS (Left "Not handled" :: Either String (Maybe Value))

exec' :: Maybe Instruction -> VmState Value
exec' (Just i) =
    execInstruction i
        >>= maybe (incPc >> getNextInstruction >>= exec') return
exec' Nothing = return $ N 0

exec :: VmState Value
exec = registerL operators >> getNextInstruction >>= exec'

-- exec v =
--     exec' v
--         >>= ( \x -> return $ case x of
--                 Right (Just v') -> Right v'
--                 Right Nothing -> Left "No result found"
--                 Left e -> Left e
--             )
--             . (result <$>)

-- exec mem args (Call key : is) stack =
-- case getFromMem mem key of
-- Just (Left (Op f)) -> do
-- case f stack of
-- Right res -> exec mem args is (res : drop 2 stack)
-- Left err -> return $ Left err
-- Just (Left (Bi code)) -> do
-- res <- exec mem stack code []
-- case res of
-- Right val -> exec mem args is (val : stack)
-- Left err -> return $ Left err
-- Just (Right f) -> do
-- r <- f stack
-- case r of
-- Right val -> exec mem args is (val : stack)
-- Left err -> return $ Left err
-- _ -> return $ Left ("Call on invalid or missing key: " ++ key)
-- exec mem args (JumpIfFalse (Left n) : is) (B b : stack)
-- \| not b = exec mem args (drop n is) stack
-- \| otherwise = exec mem args is stack
-- exec mem args (Jump (Left n) : is) stack = exec mem args (drop n is) stack
-- exec _ _ (JumpIfFalse _ : _) _ = return $ Left "JumpIfFalse needs a bool on the stack"
