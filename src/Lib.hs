{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Lib
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lib
    (   glados,
        printTree,
        sexprToAST,
        evalAST,
        SExpr (Integer, Symbol, List)
    ) where
import Data.Maybe (fromJust)
import GHC.GHCi.Helpers (flushAll)
import System.IO (isEOF, hIsTerminalDevice, stdin)

data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    deriving Show

data Ast = Define String Ast
    | Call String Ast Ast -- function param1 param2
    | AstInt Int
    | AstSymbol String (Maybe Ast) -- name value

instance Show Ast where
    show (AstInt i) = show i
    show (AstSymbol n s) = '(' : n ++ " = " ++ show s ++ ")"
    show (Define a b) = "Define " ++ show a ++ " = " ++ show b
    show (Call f a b) = "Call " ++ show a ++ " " ++ f ++ " " ++ show b

astPlus :: Ast -> Ast -> Maybe Ast
astPlus (AstInt i1) (AstInt i2) = Just (AstInt (i1 + i2))
astPlus _ _ = Nothing

astMinus :: Ast -> Ast -> Maybe Ast
astMinus (AstInt i1) (AstInt i2) = Just (AstInt (i1 - i2))
astMinus _ _ = Nothing

astMul :: Ast -> Ast -> Maybe Ast
astMul (AstInt i1) (AstInt i2) = Just (AstInt (i1 * i2))
astMul _ _ = Nothing

astDiv :: Ast -> Ast -> Maybe Ast
astDiv (AstInt i1) (AstInt i2) = Just (AstInt (i1 `div` i2))
astDiv _ _ = Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer i) = Just (AstInt i)
sexprToAST (Symbol s) = Just (AstSymbol s Nothing)
sexprToAST (List [Symbol "define", Symbol s, i]) = Just (Define s)
    <*> sexprToAST i
sexprToAST (List (Symbol "define":_)) = Nothing
sexprToAST (List [Symbol "+", i1, i2]) = Just (Call "+") <*> sexprToAST i1
    <*> sexprToAST i2
sexprToAST (List (Symbol "+":_)) = Nothing
sexprToAST (List [Symbol "-", i1, i2]) = Just (Call "-") <*> sexprToAST i1
    <*> sexprToAST i2
sexprToAST (List (Symbol "-":_)) = Nothing
sexprToAST (List [Symbol "*", i1, i2]) = Just (Call "*") <*> sexprToAST i1
    <*> sexprToAST i2
sexprToAST (List (Symbol "*":_)) = Nothing
sexprToAST (List [Symbol "/", i1, i2]) = Just (Call "/") <*> sexprToAST i1
    <*> sexprToAST i2
sexprToAST (List (Symbol "/":_)) = Nothing
sexprToAST (List _) = Nothing

evalAST :: Ast -> Maybe Ast
evalAST (Define a b) = Just (AstSymbol a (Just b))
evalAST (Call "+" a b) = astPlus <$> evalAST a <*> evalAST b
    >>= Just . fromJust
evalAST (Call "-" a b) = astMinus <$> evalAST a <*> evalAST b
    >>= Just . fromJust
evalAST (Call "*" a b) = astMul <$> evalAST a <*> evalAST b
    >>= Just . fromJust
evalAST (Call "/" a b) = astDiv <$> evalAST a <*> evalAST b
    >>= Just . fromJust
evalAST (Call {}) = Nothing
evalAST (AstInt i) = Just (AstInt i)
evalAST (AstSymbol _ Nothing) = Nothing
evalAST (AstSymbol _ s) = s

{-getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Integer i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List l) = Just l
getList _ = Nothing-}

printTree :: SExpr -> Maybe String
printTree (Symbol s) = Just ("a " ++ show (Symbol s))
printTree (Integer i) = Just ("an " ++ show (Integer i))
printTree (List [x]) = printTree x
printTree (List (x:xs)) = (\ a b -> a ++ ", " ++ b) <$> printTree x
    <*> printTree (List xs)
printTree (List []) = Nothing

getContentFromFile :: String -> IO String
getContentFromFile = readFile

-- Prints prompt if it's a TTY
getLineFromStdin :: Bool -> IO String
getLineFromStdin True = putStr "> " >> flushAll >> isEOF
    >>= \ end -> case end of
        True -> return ""
        False -> (\ a b -> a ++ "\n" ++ b)
            <$> getLine <*> getLineFromStdin True
getLineFromStdin False = isEOF >>= \ end -> case end of
        True -> return ""
        False -> (\ a b -> a ++ "\n" ++ b)
            <$> getLine <*> getLineFromStdin False

getContentFromStdin :: IO String
getContentFromStdin = hIsTerminalDevice stdin >>= getLineFromStdin

glados :: Maybe String -> IO ()
glados (Just filepath) = do
    content <- getContentFromFile filepath
    print content
glados Nothing = do
    content <- getContentFromStdin
    print content
