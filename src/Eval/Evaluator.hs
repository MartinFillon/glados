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

-- evalLambda :: [String] -> Ast -> [Ast] -> Either String Ast
-- evalLambda params body args
--     | length params == length args = do
--         evaluatedArgs <- mapM evalAST args
--         let substitutions = zip params evaluatedArgs
--         evalAST (substitute body substitutions)
--     | otherwise = Left "Lambda argument count mismatch"
evalLambda :: [String] -> Ast -> [Ast] -> Either String Ast
evalLambda params body args
    | length params == length args = 
        mapM evalAST args >>= \evaluatedArgs -> evalAST (substitute body (zip params evaluatedArgs))
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

evalAST :: Ast -> Either String Ast
evalAST (Define n expr) = Right (AstSymbol n (Just expr))
-- evalAST (Apply func args) =
--     evalAST func >>= \funcEval ->
--         fmap (\f -> evalLambda (params f) (body f) args) funcEval
--         >>= either (const (Left "Apply expects a lambda function")) id
evalAST (Apply func args) = do
    funcEval <- evalAST func
    case funcEval of
        Lambda params body -> evalLambda params body args
        _ -> Left "Apply expects a lambda function"

evalAST (Lambda params body) = Right (Lambda params body)
-- evalAST (Call (Function n args)) =
--     (Map.lookup n defaultRegistry >>= (\f -> mapM evalAST args >>= f)) 
--         `orElse` evalAST (AstSymbol n Nothing) >>= \func ->
--             case func of
--                 Lambda params body -> evalLambda params body args
--                 _ -> Left ("Undefined function: " ++ n)
evalAST (Call (Function n args)) =
    case Map.lookup n defaultRegistry of
        Just f -> do
            evaluatedArgs <- mapM evalAST args
            f evaluatedArgs
        Nothing -> do
            func <- evalAST (AstSymbol n Nothing)
            case func of
                Lambda params body -> evalLambda params body args
                _ -> Left $ "Undefined function: " ++ n

evalAST (AstFloat f) = Right (AstFloat f)
evalAST (AstInt i) = Right (AstInt i)
evalAST (AstBool b) = Right (AstBool b)
evalAST (AstSymbol s Nothing) = Right (AstSymbol s Nothing)
evalAST (AstSymbol _ (Just val)) = evalAST val
