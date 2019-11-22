module Interpreter where

import Data.Maybe
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import System.IO.Unsafe
import qualified Debug.Trace

import Ast

type Scope = Map.Map Name Expr
data Environment = Environment Scope (Maybe Environment) deriving (Show)

trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    Debug.Trace.traceIO string
    return expr

runProgram :: Program -> [Expr]
runProgram program = 
        tail $ map snd $ scanl step (initEnv, initExpr) program
    where
        step :: (Environment, Expr) -> Form -> (Environment, Expr)
        step (env, _) form = evalForm env form 

        initEnv = Environment Map.empty Nothing
        initExpr = None

runBody :: Environment -> Body -> (Environment, Expr)
runBody env body = foldl step (env, initExpr) body
    where
        step :: (Environment, Expr) -> Form -> (Environment, Expr)
        step (env, _) form = evalForm env form 
        initExpr = None

runExprs :: Environment -> [Expr] -> (Environment, [Expr])
runExprs env body = (endEnv, tail $ map snd $ evalSteps)
    where
        step :: (Environment, Expr) -> Expr -> (Environment, Expr)
        step (env, _) expr = evalExpr env expr 
        initExpr = None
        evalSteps = scanl step (env, initExpr) body
        (endEnv, _) = last evalSteps

recoverWith :: Maybe a -> Maybe a -> Maybe a
recoverWith m1@(Just x) _ = m1
recoverWith _ m2 = m2

resolve :: Environment -> Name -> Maybe Expr
resolve (Environment scope maybeParent) name =
    recoverWith (scope Map.!? name) fallback
    where fallback = maybeParent >>= (flip resolve name)

define :: Environment -> Name -> Expr -> Environment
define (Environment scope parent) name value = Environment (Map.insert name value scope) parent

replace :: Environment -> Name -> Expr -> Environment
replace env@(Environment scope maybeParent) name value = 
    let 
        replaceRec :: Maybe Expr -> Maybe Environment -> Environment
        replaceRec (Just z) _ = Environment (Map.insert name value scope) maybeParent
        replaceRec Nothing (Just parent) = Environment scope (Just $ replace parent name value)
        replaceRec Nothing Nothing = error $ name ++ " is undefined"
    in
        replaceRec (scope Map.!? name) maybeParent

evalForm :: Environment -> Form -> (Environment, Expr)
evalForm env (ExprForm expr) = evalExpr env expr
evalForm env (DefForm def) = evalDef env def

evalDef :: Environment -> Definition -> (Environment, Expr)
evalDef env (VarDef name expr) = (define env name evaled, Var name)
    where (_, evaled) = evalExpr env expr

evalDef env (FunDef name args body) = 
    (define env name expr, Var name)
    where expr = Lambda args body

evalExpr :: Environment -> Expr -> (Environment, Expr)
evalExpr env None = (env, None)
evalExpr env const@(Const _) = (env, const)

evalExpr env (Var name) = (env, value)
    where value = case (resolve env name) of
                    Just x -> x
                    _ -> error $ name ++ " is undefined"

evalExpr env (Set name expr) = (replace newEnv name evaled, None)
    where (newEnv, evaled) = evalExpr env expr
                
evalExpr env lambda@(Lambda _ _) = (env, lambda)

evalExpr env (Application fun args) = 
    let
        (env2, evaledFun) = evalExpr env fun
        (argNames, body) = case evaledFun of
            Lambda argNames body -> (argNames, body)
            _ -> error "Not applicable"
        (env3, evalArgs) = runExprs env2 args
        lambdaScope = Map.fromList $ zip argNames evalArgs
        lambdaEnv = Environment lambdaScope (Just env3)
        (Environment _ envResult, result) = runBody lambdaEnv body
    in
        (fromJust envResult, result)

evalExpr env quote@(Quote _) = (env, quote)