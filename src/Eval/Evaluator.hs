{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
-- {-# LANGUAGE LambdaCase #-}

module Eval.Evaluator (evalAST) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- import Debug.Trace (trace)
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
import Memory (Memory, readMemory, updateMemory)
import Parsing.SExprToAst (Ast (..), Function (..))

type FunctionRegistry = Map.Map String (Memory -> [Ast] -> Either String (Ast, Memory))

evalIf :: Memory -> [Ast] -> Either String (Ast, Memory)
evalIf mem [AstBool True, trueExpr, _] = evalAST mem trueExpr
evalIf mem [AstBool False, _, falseExpr] = evalAST mem falseExpr
evalIf _ _ = Left "Invalid arguments to `if`"

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

evalAndAccumulate :: (Memory, [Ast]) -> Ast -> Either String (Memory, [Ast])
evalAndAccumulate (mem, acc) arg = do
    (evaluatedArg, newMem) <- evalAST mem arg
    Right (newMem, acc ++ [evaluatedArg])

-- to fix

evalLambda :: Memory -> [String] -> Ast -> [Ast] -> Either String (Ast, Memory)
evalLambda mem params body lambdaArgs =
    if length params /= length lambdaArgs
        then Left "Lambda argument count mismatch"
        else
            let substitutions = zip params lambdaArgs
                substitutedBody = substitute body substitutions
             in evalAST mem substitutedBody

handleSymbolFunctionCall :: Memory -> String -> [Ast] -> Ast -> Either String (Ast, Memory)
handleSymbolFunctionCall mem _ evalArgs func =
    case func of
        Lambda params body -> evalLambda mem params body evalArgs
        _ -> Left "Cannot call a non-lambda value"

evalAST :: Memory -> Ast -> Either String (Ast, Memory)
evalAST mem (Define n expr) =
    evalAST mem expr >>= \(evaluatedExpr, updatedMem) ->
        Right (AstSymbol n (Just evaluatedExpr), updateMemory updatedMem n evaluatedExpr)
evalAST mem (Apply func evalArgs) =
    evalAST mem func >>= \(evaluatedFunc, memAfterFunc) ->
        case evaluatedFunc of
            Lambda params body ->
                foldM evalArgsWithMem ([], memAfterFunc) evalArgs >>= \(evaluatedArgs, memAfterArgs) ->
                    evalAST memAfterArgs (substitute body (zip params evaluatedArgs))
            _ -> Left "Apply expects a lambda function"
  where
    evalArgsWithMem (accArgs, curMem) arg =
        evalAST curMem arg >>= \(evaluatedArg, newMem) ->
            Right (accArgs ++ [evaluatedArg], newMem)
evalAST mem (Lambda params body) = Right (Lambda params body, mem)
evalAST mem (Call (Function n evalArgs)) =
    case Map.lookup n defaultRegistry of
        Just f ->
            foldM evalAndAccumulate (mem, []) evalArgs >>= \(newMem, evaluatedArgs) ->
                case f newMem evaluatedArgs of
                    Right (result, _) -> Right (result, newMem)
                    Left err -> Left err
        Nothing ->
            case readMemory mem n of
                Just (Lambda params body) ->
                    evalLambda mem params body evalArgs
                Just otherAst ->
                    handleSymbolFunctionCall mem n evalArgs otherAst
                Nothing ->
                    Left $ "Function not found: " ++ n
evalAST mem (AstFloat f) = Right (AstFloat f, mem)
evalAST mem (AstInt i) = Right (AstInt i, mem)
evalAST mem (AstBool b) = Right (AstBool b, mem)
evalAST mem (AstSymbol s Nothing) =
    maybe (Right (AstSymbol s Nothing, mem)) (\val -> Right (val, mem)) (readMemory mem s)
evalAST mem (AstSymbol _ (Just val)) = evalAST mem val
