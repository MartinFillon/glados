{-
-- EPITECH PROJECT, 2024
-- src [WSL: Ubuntu]
-- File description:
-- Ast
-}

module AstProto.Ast () where

import Data.Maybe

-- struct SExpr
data SExpr
  = SInt Int
  | SSymbol String
  | SList [SExpr]
  deriving (Show, Eq)

-- Extract symbol
getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

-- Extract int
getInteger :: SExpr -> Maybe Int
getInteger (SInt i) = Just i
getInteger _ = Nothing

-- Extract list
getList :: SExpr -> Maybe [SExpr]
getList (SList l) = Just l
getList _ = Nothing

-- printTree SExpr
printTree :: SExpr -> String
printTree (SInt i) = "a Number " ++ show i
printTree (SSymbol s) = "a Symbol '" ++ s ++ "'"
printTree (SList l) = "a List with " ++ unwords (map printTree l)

-- AST
data AST
  = Define String AST
  | Call String [AST]
  | Lambda [String] AST
  | AInt Int
  | ASymbol String
  | ABool Bool
  deriving (Show, Eq)

-- Convert SExp -> AST
sexprToAST :: SExpr -> Maybe AST
sexprToAST (SInt i) = Just (AInt i)
sexprToAST (SSymbol s) = Just (ASymbol s)
-- Special case for define
sexprToAST (SList [SSymbol "define", SSymbol var, value]) =
  case sexprToAST value of
    Just astValue -> Just (Define var astValue)
    Nothing -> Nothing
-- Special case for lambda
-- body should be list
sexprToAST (SList [SSymbol "lambda", SList params, body@(SList _)]) =
  let paramNames = mapMaybe getSymbol params
   in case sexprToAST body of
        Just bodyAST -> Just (Lambda paramNames bodyAST)
        Nothing -> Nothing
-- Call Function
sexprToAST (SList (SSymbol func : args)) =
  case sequence (map sexprToAST args) of
    Just astArgs -> Just (Call func astArgs)
    Nothing -> Nothing
sexprToAST _ = Nothing

-- data Memory = Memory
--   { vars :: [(String, AST)],
--     lastResult :: AST
--   }

type Memory = [(String, AST)];

-- EvaL AST
evalAST :: Memory -> AST -> Maybe AST
evalAST _ (AInt i) = Just (AInt i)
-- Variable evel
evalAST mem (ASymbol name) = lookup name mem
-- Define eval
evalAST mem (Define _ value) = evalAST mem value
-- Lambda eval
evalAST _ (Lambda params body) = Just (Lambda params body)
-- Call eval
evalAST mem (Call op args) =
  case mapM (evalAST mem) args of
    Just argv -> applyOp op argv
    Nothing -> Nothing
evalAST _ _ = Nothing

execAST :: AST -> Memory -> Memory
execAST (Define name value) mem =
  case evalAST mem value of
    Just val -> (name, val): mem
    Nothing -> mem
execAST _ _ = []

-- Apply basic operation
applyOp :: String -> [AST] -> Maybe AST
applyOp "+" [AInt a, AInt b] = Just (AInt (a + b))
applyOp "*" [AInt a, AInt b] = Just (AInt (a * b))
applyOp "-" [AInt a, AInt b] = Just (AInt (a - b))
applyOp "%" [AInt a, AInt b] = Just (AInt (a `mod` b))
applyOp "/" [AInt a, AInt b] = Just (AInt (a `div` b))
applyOp _ _ = Nothing
