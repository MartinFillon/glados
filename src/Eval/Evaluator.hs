{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}

module Eval.Evaluator (evalAST) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

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

evalIf :: Memory -> [Ast] -> Either String (Ast, Memory)
evalIf mem [AstBool True, trueExpr, _] = 
    evalAST mem trueExpr
evalIf mem [AstBool False, _, falseExpr] = 
    evalAST mem falseExpr
evalIf _ _ = Left "Invalid arguments to `if`"

------------

substitute :: Ast -> [(String, Ast)] -> Memory -> Ast
substitute (Define n value) subs mem =
    Define n (substitute value subs mem)
substitute (AstSymbol n _) subs mem =
    case lookup n subs of
        Just val -> 
            trace ("substitute1 --> symbol n: " ++ show n ++ " value: " ++ show val) $
            val
        Nothing -> 
            case Map.lookup n mem of  -- Use Map.lookup for memory lookup
                Just valInMem -> 
                    trace ("substitute --> symbol n: " ++ show n ++ " value from memory: " ++ show valInMem) $
                    valInMem
                Nothing -> 
                    AstSymbol n AstVoid
substitute (Call (Function n subArgs)) subs mem =
    -- trace ("substitute --> call function: " ++ show n ++ " a: " ++ show a) $
     case lookup n subs of
        Just _ -> 
            trace ("callsubstitute1 --> recursive call detected for: " ++ show n) $
            Call (Function n subArgs)  -- Return the Call as is (prevent further substitution)
        Nothing -> -- Substitute arguments inside the Call expression
            trace ("callsubstitute1 --> call function: " ++ show n) $
            Call (Function n (map (\arg -> substitute arg subs mem) subArgs))
substitute (Lambda params body) subs mem =
    Lambda params (substitute body subs mem)  -- No substitution inside lambda's params
substitute other _ _ = other

evalAndAccumulate :: (Memory, [Ast]) -> Ast -> Either String (Memory, [Ast])
evalAndAccumulate (mem, acc) arg =
    evalAST mem arg >>= \(evaluatedArg, newMem) ->
        Right (newMem, acc ++ [evaluatedArg])

evalLambda :: Memory -> [String] -> Ast -> [Ast] -> Either String (Ast, Memory)
evalLambda mem params body lambdaArgs
    | length params /= length lambdaArgs = Left "Lambda argument count mismatch"
    | otherwise = evalAST mem substitutedBody
  where
    substitutedBody = substitute body (zip params lambdaArgs) mem

handleSymbolFunctionCall :: Memory -> String -> [Ast] -> Ast -> Either String (Ast, Memory)
handleSymbolFunctionCall mem _ evalArgs (Lambda params body) =
    evalLambda mem params body evalArgs
handleSymbolFunctionCall _ _ _ _ =
    Left "Cannot call a non-lambda value"

evalAST :: Memory -> Ast -> Either String (Ast, Memory)
evalAST mem (Define n expr) =
    evalAST mem expr >>= \(evaluatedExpr, updatedMem) ->
        case evaluatedExpr of
            Lambda _ _ -> Right (AstVoid, updateMemory updatedMem n evaluatedExpr)
            _ -> Right (AstVoid, updateMemory updatedMem n evaluatedExpr)
evalAST mem (Apply func evalArgs) =
    evalAST mem func >>= \(evaluatedFunc, memAfterFunc) ->
        case evaluatedFunc of
            Lambda params body -> 
                foldM evalArgsWithMem ([], memAfterFunc) evalArgs >>= \(evaluatedArgs, memAfterArgs) ->
                    evalAST memAfterArgs (substitute body (zip params evaluatedArgs) mem)
            _ -> Left "Apply expects a lambda function"
  where
    evalArgsWithMem (accArgs, curMem) arg =
        evalAST curMem arg >>= \(evaluatedArg, newMem) ->
            Right (accArgs ++ [evaluatedArg], newMem)
evalAST mem (Lambda params body) =
    Right (Lambda params body, mem)
evalAST mem (Call (Function n evalArgs)) =
    -- trace ("evaluating " ++ show n ++ " with x = " ++ show evalArgs) $
    case Map.lookup n defaultRegistry of
        Just f ->
            foldM evalAndAccumulate (mem, []) evalArgs >>= \(newMem, evaluatedArgs) ->
                case f newMem evaluatedArgs of
                    Right (result, _) ->
                        Right (result, newMem)
                    Left err -> Left err
        Nothing ->
            case readMemory mem n of
                Just (Lambda params body) ->
                    evalLambda mem params body evalArgs
                Just otherAst ->
                    handleSymbolFunctionCall mem n evalArgs otherAst
                Nothing ->
                    Left $ "Function " ++ show n ++ " not defined"
evalAST mem (AstFloat f) =
    Right (AstFloat f, mem)
evalAST mem (AstInt i) =
    Right (AstInt i, mem)
evalAST mem (AstBool b) =
    Right (AstBool b, mem)
evalAST mem AstVoid = Right (AstVoid, mem)
evalAST mem (AstSymbol s AstVoid) =
    maybe (Right (AstSymbol s AstVoid, mem)) (\val -> Right (val, mem)) (readMemory mem s)
evalAST mem (AstSymbol _ val) =
    evalAST mem val
