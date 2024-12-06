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

type FunctionRegistry = Map.Map String ([Ast] -> Either String Ast)

-- evalLambda :: [String] -> Ast -> [Ast] -> Either String Ast
-- evalLambda params body evalArgs
--     | length params == length evalArgs =
--         mapM evalAST evalArgs >>= \evaluatedArgs -> evalAST (substitute body (zip params evaluatedArgs))
--     | otherwise = Left "Lambda argument count mismatch"

-- evalIf :: [Ast] -> Either String Ast
-- evalIf [AstBool True, trueExpr, _] = evalAST trueExpr
-- evalIf [AstBool False, _, falseExpr] = evalAST falseExpr
-- evalIf _ = Left "Invalid arguments to `if`"

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
          ("not", evalNot)
          --   ("if", evalIf)
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

-- handleSymbolFunctionCall :: String -> [Ast] -> Ast -> Either String Ast
-- handleSymbolFunctionCall n evalArgs func =
--     case func of
--         Lambda params body -> evalLambda params body evalArgs
--         _ -> Left $ "Undefined function: " ++ n

-- evalAST :: Ast -> Either String Ast
-- evalAST (Define n expr) = Right (AstSymbol n (Just expr))
-- evalAST (Apply func evalArgs) =
--     evalAST func >>= \case
--         Lambda params body -> evalLambda params body evalArgs
--         _ -> Left "Apply expects a lambda function"
-- evalAST (Lambda params body) = Right (Lambda params body)
-- evalAST (Call (Function n evalArgs)) =
--     case Map.lookup n defaultRegistry of
--         Just f -> mapM evalAST evalArgs >>= f
--         Nothing -> evalAST (AstSymbol n Nothing) >>= handleSymbolFunctionCall n evalArgs
-- evalAST (AstFloat f) = Right (AstFloat f)
-- evalAST (AstInt i) = Right (AstInt i)
-- evalAST (AstBool b) = Right (AstBool b)
-- evalAST (AstSymbol s Nothing) = Right (AstSymbol s Nothing)
-- evalAST (AstSymbol _ (Just val)) = evalAST val

evalAndAccumulate :: (Memory, [Ast]) -> Ast -> Either String (Memory, [Ast])
evalAndAccumulate (mem, acc) arg = do
    (evaluatedArg, newMem) <- evalAST mem arg
    Right (newMem, acc ++ [evaluatedArg])

evalLambda :: Memory -> [String] -> Ast -> [Ast] -> Either String (Ast, Memory)
evalLambda mem params body args =
    if length params /= length args
        then Left "Lambda argument count mismatch"
        else
            let substitutions = zip params args
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
                case f evaluatedArgs of
                    Right result -> Right (result, newMem)
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
