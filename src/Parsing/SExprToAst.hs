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

import qualified Data.Map as Map
import Control.Applicative (Applicative(liftA2))

data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    | Boolean Bool
    deriving Show

data Function = Function {
    name :: String,
    args :: [Ast]
}

data Ast = Define String Ast
    | Call Function -- function param1 param2
    | AstInt Int
    | AstBool Bool
    | AstSymbol String (Maybe Ast) -- name value
    | Lambda [String] Ast
    | Apply Ast [Ast]

type FunctionRegistry = Map.Map String ([Ast] -> Maybe Ast)

instance Show Ast where
    show (AstInt i) = show i
    show (AstBool b) = if b then "#t" else "#f"
    show (AstSymbol n s) = '(' : n ++ " = " ++ show s ++ ")"
    show (Define a b) = "Define " ++ show a ++ " = " ++ show b
    show (Call (Function n a)) = "Call " ++ n ++ concatMap (\ x -> ' ' : show x) a

instance Eq Ast where
    (AstInt i1) == (AstInt i2) = i1 == i2
    (AstBool b1) == (AstBool b2) = b1 == b2
    (AstSymbol n1 s1) == (AstSymbol n2 s2) = n1 == n2 && s1 == s2
    (Define n1 a1) == (Define n2 a2) = n1 == n2 && a1 == a2
    (Call (Function n1 a1)) == (Call (Function n2 a2)) = n1 == n2 && a1 == a2
    (Lambda p1 b1) == (Lambda p2 b2) = p1 == p2 && b1 == b2
    _ == _ = False


getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _ = Nothing

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
printTree (Boolean b) = Just ("Bool: " ++ if b then "#t" else "#f")
printTree (List [x]) = printTree x
printTree (List (x:xs)) = (\ a b -> a ++ ", " ++ b) <$> printTree x
    <*> printTree (List xs)
printTree (List []) = Nothing


------------ OP

astPlus :: [Ast] -> Maybe Ast
astPlus [AstInt i1, AstInt i2] = Just (AstInt (i1 + i2))
astPlus _ = Nothing

astMinus :: [Ast] -> Maybe Ast
astMinus [AstInt i1, AstInt i2] = Just (AstInt (i1 - i2))
astMinus _ = Nothing

astMul :: [Ast] -> Maybe Ast
astMul [AstInt i1, AstInt i2] = Just (AstInt (i1 * i2))
astMul _ = Nothing

astDiv :: [Ast] -> Maybe Ast
astDiv [AstInt i1, AstInt i2]
    | i2 /= 0 = Just (AstInt (i1 `div` i2))
    | otherwise = Nothing
astDiv _ = Nothing

astMod :: [Ast] -> Maybe Ast
astMod [AstInt i1, AstInt i2]
    | i2 /= 0 = Just (AstInt (i1 `mod` i2))
    | otherwise = Nothing
astMod _ = Nothing

astLt :: [Ast] -> Maybe Ast
astLt [AstInt i1, AstInt i2] = Just (AstBool (i1 < i2))
astLt _ = Nothing

astEq :: [Ast] -> Maybe Ast
astEq [AstInt i1, AstInt i2] = Just (AstBool (i1 == i2))
astEq [AstBool b1, AstBool b2] = Just (AstBool (b1 == b2))
astEq [AstSymbol s1 _, AstSymbol s2 _] = Just (AstBool (s1 == s2))
astEq _ = Nothing

astGt :: [Ast] -> Maybe Ast
astGt [AstInt i1, AstInt i2] = Just (AstBool (i1 > i2))
astGt _ = Nothing

---------- bool OP

astAnd :: [Ast] -> Maybe Ast
astAnd [AstBool b1, AstBool b2] = Just (AstBool (b1 && b2))
astAnd _ = Nothing

astOr :: [Ast] -> Maybe Ast
astOr [AstBool b1, AstBool b2] = Just (AstBool (b1 || b2))
astOr _ = Nothing

astNot :: [Ast] -> Maybe Ast
astNot [AstBool b] = Just (AstBool (not b))
astNot _ = Nothing

astIf :: [Ast] -> Maybe Ast
astIf [AstBool cond, trueExpr, falseExpr] =
    if cond then evalAST trueExpr else evalAST falseExpr
astIf _ = Nothing

---------- Lambda eval

evalLambda :: [String] -> Ast -> [Ast] -> Maybe Ast
evalLambda params body args
    | length params == length args = do
        evaluatedArgs <- mapM evalAST args
        let substitutions = zip params evaluatedArgs
        evalAST (substitute body substitutions)
    | otherwise = Nothing


substitute :: Ast -> [(String, Ast)] -> Ast
substitute (AstSymbol name Nothing) subs =
    maybe (AstSymbol name Nothing) id (lookup name subs)
substitute (Define name value) subs =
    Define name (substitute value subs)
substitute (Call (Function n args)) subs =
    Call (Function n (map (`substitute` subs) args))
substitute (Lambda params body) subs =
    Lambda params (substitute body subs) -- No substitution inside lambda's params
substitute other _ = other


---------------- Registry function

defaultRegistry :: FunctionRegistry
defaultRegistry = Map.fromList [
    ("+", astPlus),
    ("-", astMinus),
    ("*", astMul),
    ("/", astDiv),
    ("and", astAnd),
    ("or", astOr),
    ("not", astNot),
    ("if", astIf),
    ("%", astMod),
    ("<", astLt),
    ("eq?", astEq),
    (">", astGt)
  ]


------------ Sexpr -> AST

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer i) = Just (AstInt i)
sexprToAST (Boolean b) = Just (AstBool b)
sexprToAST (Symbol s) = Just (AstSymbol s Nothing)

sexprToAST (List [Symbol "define", Symbol s, expr]) =
    Define s <$> sexprToAST expr
-- Adjust sexprToAST to handle lambda expressions
sexprToAST (List [Symbol "lambda", List params, body]) = do
    paramNames <- mapM getSymbol params
    bodyAst <- sexprToAST body
    return (Lambda paramNames bodyAst)
-- Adjust sexprToAST to handle function applications
sexprToAST (List (func : args)) = do
    funcAst <- sexprToAST func
    argAsts <- mapM sexprToAST args
    return (Apply funcAst argAsts)

sexprToAST (List (Symbol op : args)) = do
    parsedArgs <- mapM sexprToAST args
    return (Call (Function op parsedArgs))


sexprToAST _ = Nothing


-----------

evalAST :: Ast -> Maybe Ast
evalAST (Define name expr) = Just (AstSymbol name (evalAST expr))
evalAST (Apply func args) = do
    funcEval <- evalAST func
    case funcEval of
        Lambda params body ->
            if length params == length args then do
                evaluatedArgs <- mapM evalAST args
                let substitutions = zip params evaluatedArgs
                evalAST (substitute body substitutions)
            else
                Nothing
        _ -> Nothing
evalAST (Lambda params body) = Just (Lambda params body)
evalAST (Call (Function n args)) =
    case Map.lookup n defaultRegistry of
        Just f -> do
            evaluatedArgs <- mapM evalAST args
            f evaluatedArgs
        Nothing -> do
            func <- evalAST (AstSymbol n Nothing)
            case func of
                Lambda params body -> evalLambda params body args
                _ -> Nothing
evalAST (AstInt i) = Just (AstInt i)
evalAST (AstBool b) = Just (AstBool b)
evalAST (AstSymbol s Nothing) = Just (AstSymbol s Nothing)
evalAST (AstSymbol _ (Just val)) = evalAST val


