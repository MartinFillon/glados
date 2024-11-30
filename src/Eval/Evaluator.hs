{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}

module Eval.Evaluator (evalAST) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Eval.Boolean (
    evalAnd,
    evalEq,
    evalGt,
    evalLt,
    evalNot,
    evalOr,
 )
import Eval.Maths (
    evalAdd,
    evalDiv,
    evalMod,
    evalMul,
    evalSub,
 )
import Parsing.SExprToAst (Ast (..), Function (..))

type FunctionRegistry = Map.Map String ([Ast] -> Maybe Ast)

evalLambda :: [String] -> Ast -> [Ast] -> Maybe Ast
evalLambda params body a
    | length params == length a = do
        evaluatedArgs <- mapM evalAST a
        let substitutions = zip params evaluatedArgs
        evalAST (substitute body substitutions)
    | otherwise = Nothing

evalIf :: [Ast] -> Maybe Ast
evalIf [AstBool True, trueExpr, _] = evalAST trueExpr
evalIf [AstBool False, _, falseExpr] = evalAST falseExpr
evalIf _ = Nothing

defaultRegistry :: FunctionRegistry
defaultRegistry =
    Map.fromList
        [ ("+", evalAdd),
          ("-", evalSub),
          ("*", evalMul),
          ("/", evalDiv),
          ("div", evalDiv),
          ("%", evalMod),
          ("mod", evalMod),
          ("eq?", evalEq),
          ("<", evalLt),
          (">", evalGt),
          ("and", evalAnd),
          ("or", evalOr),
          ("not", evalNot),
          ("if", evalIf)
        ]

substitute :: Ast -> [(String, Ast)] -> Ast
substitute (AstSymbol n Nothing) subs =
    fromMaybe (AstSymbol n Nothing) (lookup n subs)
substitute (Define n value) subs =
    Define n (substitute value subs)
substitute (Call (Function n a)) subs =
    Call (Function n (map (`substitute` subs) a))
substitute (Lambda params body) subs =
    Lambda params (substitute body subs) -- No substitution inside lambda's params
substitute other _ = other

evalAST :: Ast -> Maybe Ast
evalAST (Define n expr) = Just (AstSymbol n (evalAST expr))
evalAST (Apply func a) = do
    funcEval <- evalAST func
    case funcEval of
        Lambda params body ->
            if length params == length a
                then do
                    evaluatedArgs <- mapM evalAST a
                    let substitutions = zip params evaluatedArgs
                    evalAST (substitute body substitutions)
                else Nothing
        _ -> Nothing
evalAST (Lambda params body) = Just (Lambda params body)
evalAST (Call (Function n x)) =
    case Map.lookup n defaultRegistry of
        Just f -> do
            evaluatedArgs <- mapM evalAST x
            f evaluatedArgs
        Nothing -> do
            func <- evalAST (AstSymbol n Nothing)
            case func of
                Lambda params body -> evalLambda params body x
                _ -> Nothing
evalAST (AstInt i) = Just (AstInt i)
evalAST (AstBool b) = Just (AstBool b)
evalAST (AstFloat f) = Just (AstFloat f)
evalAST (AstSymbol s Nothing) = Just (AstSymbol s Nothing)
evalAST (AstSymbol _ (Just val)) = evalAST val
