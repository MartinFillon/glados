{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- SExprToAst
-}

module Parsing.SExprToAst
    (   printTree,
        sexprToAST,
        evalAST,
        SExpr (Integer, Symbol, List)
    ) where
import Control.Applicative (Applicative(liftA2))

data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    deriving Show

data Function = Function {
    name :: String,
    args :: [Ast]
}

data Ast = Define String Ast
    | Call Function -- function param1 param2
    | AstInt Int
    | AstSymbol String (Maybe Ast) -- name value

instance Show Ast where
    show (AstInt i) = show i
    show (AstSymbol n s) = '(' : n ++ " = " ++ show s ++ ")"
    show (Define a b) = "Define " ++ show a ++ " = " ++ show b
    show (Call (Function n a)) = "Call " ++ n ++ concatMap (\ x -> ' ' : show x) a

astPlus :: Maybe Ast -> Maybe Ast -> Maybe Ast
astPlus (Just (AstInt i1)) (Just (AstInt i2)) = Just (AstInt (i1 + i2))
astPlus _ _ = Nothing

astMinus :: Maybe Ast -> Maybe Ast -> Maybe Ast
astMinus (Just (AstInt i1)) (Just (AstInt i2)) = Just (AstInt (i1 - i2))
astMinus _ _ = Nothing

astMul :: Maybe Ast -> Maybe Ast -> Maybe Ast
astMul (Just (AstInt i1)) (Just (AstInt i2)) = Just (AstInt (i1 * i2))
astMul _ _ = Nothing

astDiv :: Maybe Ast -> Maybe Ast -> Maybe Ast
astDiv (Just (AstInt i1)) (Just (AstInt i2)) = Just (AstInt (i1 `div` i2))
astDiv _ _ = Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer i) = Just (AstInt i)
sexprToAST (Symbol s) = Just (AstSymbol s Nothing)
sexprToAST (List [Symbol "define", Symbol s, i]) = Just (Define s)
    <*> sexprToAST i
sexprToAST (List (Symbol "define":_)) = Nothing
sexprToAST (List [Symbol "+", i1, i2]) =
    liftA2 (,) (sexprToAST i1) (sexprToAST i2)
    >>= \ (int1, int2) -> Just (Call (Function
        { name="+"
        , args=[int1, int2] }))
sexprToAST (List (Symbol "+":_)) = Nothing
sexprToAST (List [Symbol "-", i1, i2]) = liftA2 (,) (sexprToAST i1) (sexprToAST i2)
    >>= \ (int1, int2) -> Just (Call (Function
        { name="-"
        , args=[int1, int2] }))
sexprToAST (List (Symbol "-":_)) = Nothing
sexprToAST (List [Symbol "*", i1, i2]) = liftA2 (,) (sexprToAST i1) (sexprToAST i2)
    >>= \ (int1, int2) -> Just (Call (Function
        { name="*"
        , args=[int1, int2] }))
sexprToAST (List (Symbol "*":_)) = Nothing
sexprToAST (List [Symbol "/", i1, i2]) = liftA2 (,) (sexprToAST i1) (sexprToAST i2)
    >>= \ (int1, int2) -> Just (Call (Function
        { name="/"
        , args=[int1, int2] }))
sexprToAST (List (Symbol "/":_)) = Nothing
sexprToAST (List _) = Nothing

evalAST :: Ast -> Maybe Ast
evalAST (Define a b) = Just (AstSymbol a (Just b))
evalAST (Call (Function "+" a)) = foldr (astPlus . evalAST) (Just (AstInt 0)) a
evalAST (Call (Function "-" a)) = foldr (astMinus . evalAST) (Just (AstInt 0)) a
evalAST (Call (Function "*" a)) = foldr (astMul . evalAST) (Just (AstInt 0)) a
evalAST (Call (Function "/" a)) = foldr (astDiv . evalAST) (Just (AstInt 0)) a
{-evalAST (Call "-" a b) = astMinus <$> evalAST a <*> evalAST b
    >>= Just . fromJust
evalAST (Call "*" a b) = astMul <$> evalAST a <*> evalAST b
    >>= Just . fromJust
evalAST (Call "/" a b) = astDiv <$> evalAST a <*> evalAST b
    >>= Just . fromJust-}
evalAST (Call _) = Nothing
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
