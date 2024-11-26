{-
-- EPITECH PROJECT, 2024
-- src [WSL: Ubuntu]
-- File description:
-- Ast
-}

import Data.Maybe

-- struct SExpr
data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show, Eq)


-- Extract symbol
getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _           = Nothing

-- Extract int
getInteger :: SExpr -> Maybe Int
getInteger (SInt i) = Just i
getInteger _        = Nothing

-- Extract list
getList :: SExpr -> Maybe [SExpr]
getList (SList l) = Just l
getList _         = Nothing

-- printTree SExpr
printTree :: SExpr -> String
printTree (SInt i) = "a Number " ++ show i
printTree (SSymbol s) = "a Symbol '" ++ s ++ "'"
printTree (SList l) = "a List with " ++ unwords (map printTree l)


-- AST
data AST = Define String AST
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
sexprToAST (SList [SSymbol "lambda", SList params, body]) =
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


-- EvaL AST
evalAST :: AST -> Maybe AST
evalAST (AInt i) = Just (AInt i)
-- Lambda eval
evalAST (Lambda params body) = Just (Lambda params body)
-- Call eval
evalAST (Call op args) =
  case mapM evalAST args of
    Just argv -> applyOp op argv
    Nothing -> Nothing
evalAST _ = Nothing

-- Apply basic operation
applyOp :: String -> [AST] -> Maybe AST
applyOp "+" [AInt a, AInt b] = Just (AInt (a + b))
applyOp "*" [AInt a, AInt b] = Just (AInt (a * b))
applyOp "-" [AInt a, AInt b] = Just (AInt (a - b))
applyOp "%" [AInt a, AInt b] = Just (AInt (a `mod` b))
applyOp "/" [AInt a, AInt b] = Just (AInt (a `div` b))
applyOp _ _ = Nothing

