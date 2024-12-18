{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Evaluator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Eval.Evaluator (evalAST) where

import Control.Monad (foldM)
import qualified Data.Map as Map
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

type FunctionRegistry =
    Map.Map String (Memory -> [Ast] -> Either String (Ast, Memory))

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

evalCondition :: Memory -> String -> Ast -> [Ast] -> Either String (Ast, Memory)
evalCondition m n cond x = evalArgs m [cond] >>= \a -> evalCall' m n (a ++ x)

substitute' :: Maybe Ast -> String -> Memory -> Ast
substitute' (Just val) _ _ = val
substitute' Nothing n mem = maybe AstVoid id (readMemory mem n)

substitute :: Ast -> [(String, Ast)] -> Memory -> Ast
substitute (Define n value) subs mem =
    Define n (substitute value subs mem)
substitute (AstSymbol n _) subs mem =
    substitute' (lookup n subs) n mem
substitute (Call (Function n subArgs)) subs mem =
    substituteFunction Call n subArgs subs mem
substitute (Condition (Function n subArgs)) subs mem =
    substituteFunction Condition n subArgs subs mem
substitute (Lambda params body) subs mem =
    Lambda params (substitute body subs mem)
substitute other _ _ = other

substituteFunction :: (Function -> Ast) -> String -> [Ast] -> [(String, Ast)] -> Memory -> Ast
substituteFunction constructor n subArgs subs mem =
    case lookup n subs of
        Just _ -> constructor (Function n subArgs)
        Nothing -> constructor (Function n (map (\arg -> substitute arg subs mem) subArgs))

evalLambda :: Memory -> [String] -> Ast -> [Ast] -> Either String (Ast, Memory)
evalLambda mem params body lambdaArgs
    | length params /= length lambdaArgs = Left "Lambda argument count mismatch"
    | otherwise = evalAST mem substitutedBody
  where
    substitutedBody = substitute body (zip params lambdaArgs) mem

handleSymbolFunctionCall ::
    Memory -> String -> [Ast] -> Ast -> Either String (Ast, Memory)
handleSymbolFunctionCall mem _ ag (Lambda params body) =
    evalLambda mem params body ag
handleSymbolFunctionCall _ _ _ _ =
    Left "Cannot call a non-lambda value"

evalArgs :: Memory -> [Ast] -> Either String [Ast]
evalArgs _ [] = Right []
evalArgs m (x : xs) =
    evalAST m x >>= \(evaluatedX, _) ->
        evalArgs m xs
            >>= \evaluatedArgs -> Right (evaluatedX : evaluatedArgs)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

evalDefinedFunction' ::
    Memory ->
    String ->
    [Ast] ->
    Maybe Ast ->
    Either String (Ast, Memory)
evalDefinedFunction' m _ args' (Just (Lambda params body)) =
    evalLambda m params body args'
evalDefinedFunction' m n args' (Just otherAst) =
    handleSymbolFunctionCall m n args' otherAst
evalDefinedFunction' _ n _ Nothing =
    Left $ "Function " ++ show n ++ " not defined"

evalDefinedFunction :: Memory -> String -> [Ast] -> Either String (Ast, Memory)
evalDefinedFunction m n args' = evalDefinedFunction' m n args' (readMemory m n)

evalOperator :: Memory -> String -> [Ast] -> Either String (Ast, Memory)
evalOperator m n args' =
    maybeToEither
        "Not a valid operator"
        (Map.lookup n defaultRegistry)
        >>= (\f -> f m args')

evalCall' :: Memory -> String -> [Ast] -> Either String (Ast, Memory)
evalCall' m n args' = case evalOperator m n args' of
    Right (result, mem) -> Right (result, mem)
    Left "Not a valid operator" -> evalDefinedFunction m n args'
    Left err -> Left err

evalCall :: Memory -> String -> [Ast] -> Either String (Ast, Memory)
evalCall m n args' = evalArgs m args' >>= \a -> evalCall' m n a

evalApply :: Memory -> Memory -> Ast -> [Ast] -> Either String (Ast, Memory)
evalApply m newm (Lambda params body) ag =
    foldM evalArgsWithMem ([], newm) ag >>= \(evaluatedArgs, memAfterArgs) ->
        evalAST memAfterArgs (substitute body (zip params evaluatedArgs) m)
  where
    evalArgsWithMem (accArgs, curMem) arg =
        evalAST curMem arg >>= \(evaluatedArg, newMem) ->
            Right (accArgs ++ [evaluatedArg], newMem)
evalApply _ _ _ _ = Left "Apply expects a lambda function"

evalAST :: Memory -> Ast -> Either String (Ast, Memory)
evalAST mem (Define n expr) =
    evalAST mem expr >>= \(evaluatedExpr, updatedMem) ->
        Right (AstVoid, updateMemory updatedMem n evaluatedExpr)
evalAST mem (Apply func ag) =
    evalAST mem func >>= \(evaluatedFunc, memAfterFunc) ->
        evalApply mem memAfterFunc evaluatedFunc ag
evalAST mem (Lambda params body) =
    Right (Lambda params body, mem)
evalAST mem (Condition (Function n (cond : ag))) =
    evalCondition mem n cond ag
evalAST mem (Call (Function n ag)) = evalCall mem n ag
evalAST mem (AstFloat f) =
    Right (AstFloat f, mem)
evalAST mem (AstInt i) =
    Right (AstInt i, mem)
evalAST mem (AstBool b) =
    Right (AstBool b, mem)
evalAST mem AstVoid = Right (AstVoid, mem)
evalAST mem (AstSymbol s AstVoid) =
    maybe
        (Right (AstSymbol s AstVoid, mem))
        (\val -> Right (val, mem))
        (readMemory mem s)
evalAST mem (AstSymbol _ val) =
    evalAST mem val
evalAST _ _ = Left "Invalid AST"
