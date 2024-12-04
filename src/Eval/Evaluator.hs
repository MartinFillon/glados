{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# LANGUAGE LambdaCase #-}

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

type FunctionRegistry = Map.Map String ([Ast] -> Either String Ast)

evalLambda :: [String] -> Ast -> [Ast] -> Either String Ast
evalLambda params body evalArgs
    | length params == length evalArgs = 
        mapM evalAST evalArgs >>= \evaluatedArgs -> evalAST (substitute body (zip params evaluatedArgs))
    | otherwise = Left "Lambda argument count mismatch"

evalIf :: [Ast] -> Either String Ast
evalIf [AstBool True, trueExpr, _] = evalAST trueExpr
evalIf [AstBool False, _, falseExpr] = evalAST falseExpr
evalIf _ = Left "Invalid arguments to `if`"

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

handleSymbolFunctionCall :: String -> [Ast] -> Ast -> Either String Ast
handleSymbolFunctionCall n evalArgs func =
    case func of
        Lambda params body -> evalLambda params body evalArgs
        _ -> Left $ "Undefined function: " ++ n

evalAST :: Ast -> Either String Ast
evalAST (Define n expr) = Right (AstSymbol n (Just expr))
evalAST (Apply func evalArgs) =
    evalAST func >>= \case
        Lambda params body -> evalLambda params body evalArgs
        _ -> Left "Apply expects a lambda function"
evalAST (Lambda params body) = Right (Lambda params body)
evalAST (Call (Function n evalArgs)) =
    case Map.lookup n defaultRegistry of
        Just f -> mapM evalAST evalArgs >>= f
        Nothing -> evalAST (AstSymbol n Nothing) >>= handleSymbolFunctionCall n evalArgs
evalAST (AstFloat f) = Right (AstFloat f)
evalAST (AstInt i) = Right (AstInt i)
evalAST (AstBool b) = Right (AstBool b)
evalAST (AstSymbol s Nothing) = Right (AstSymbol s Nothing)
evalAST (AstSymbol _ (Just val)) = evalAST val
