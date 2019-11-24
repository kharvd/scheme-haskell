module Interpreter where

import Data.Maybe
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as ST
import Control.Monad.Trans.Except (ExceptT (..), throwE, catchE)

import Predefs
import Expression
import Utils
import qualified Environment as Env
import Environment (Environment)

type InterpreterT m = ExceptT SchemeError (StateT (Environment Expr) m)

initEnv :: Environment Expr
initEnv = Env.create predefScope Nothing

runBody :: (Monad m) => Body -> InterpreterT m Expr
runBody = fmap last . mapM evalForm

evalForm :: (Monad m) => Form -> InterpreterT m Expr
evalForm (ExprForm expr) = evalExpr expr
evalForm (DefForm def) = evalDef def

evalDef :: (Monad m) => Definition -> InterpreterT m Expr
evalDef (VarDef name expr) = do
    evaledExpr <- evalExpr expr
    ST.modify $ Env.insert name evaledExpr
    return $ Var name

evalDef (FunDef name args body) = do
    ST.modify $ Env.insert name (Lambda args body)
    return $ Var name

withEnv :: (Monad m) => Environment Expr -> InterpreterT m Expr -> InterpreterT m Expr
withEnv env computation = do
  oldEnv <- ST.get
  let 
    handleError e = do
      ST.put oldEnv 
      throwE e
      
  ST.put env
  result <- catchE computation handleError
  innerEnv <- ST.get

  case Env.parent innerEnv of
     Just x -> ST.put x
     Nothing -> error "fatal error: empty parent environment"
  return result

applyLambda :: (Monad m) => [Name] -> Body -> [Expr] -> InterpreterT m Expr
applyLambda argNames body args =
  do
    outerEnv <- ST.get
    let lambdaEnv = Env.create (zip argNames args) (Just outerEnv)
    withEnv lambdaEnv (runBody body)

applyPredefFun :: (Monad m) => NamedFunction -> [Expr] -> InterpreterT m Expr
applyPredefFun (NamedFunction _ fun) args = ExceptT $ return (fun args)

evalExpr :: (Monad m) => Expr -> InterpreterT m Expr

evalExpr None = return None

evalExpr const@(BoolConst _) = return const
evalExpr const@(IntConst _) = return const

evalExpr (Var name) = do
    env <- ST.get
    case Env.resolve name env of
        Just x -> return x
        _ -> throwE $ SchemeError (name ++ " is undefined")

evalExpr (Set name expr) = do
    evaledExpr <- evalExpr expr
    env <- ST.get
    let maybeNewEnv = Env.replace name evaledExpr env
    case maybeNewEnv of
      Just newEnv -> ST.put newEnv
      Nothing -> throwE $ SchemeError (name ++ " is not in scope")

    return None

evalExpr lambda@(Lambda _ _) = return lambda

evalExpr (Application fun args) = do
    evaledFun <- evalExpr fun
    evaledArgs <- mapM evalExpr args
    case evaledFun of
        Lambda argNames body -> applyLambda argNames body evaledArgs
        Predef fun -> applyPredefFun fun evaledArgs
        _ -> throwE $ SchemeError "Not applicable"

evalExpr (Quote expr) = return expr

evalExpr (If cond ifTrue maybeIfFalse) = do
    condEval <- evalExpr cond
    case (condEval, maybeIfFalse) of
        (BoolConst False, Just ifFalse) -> evalExpr ifFalse
        (BoolConst False, Nothing) -> return None
        _ -> evalExpr ifTrue

evalExpr (And exprs) = let
    evalAnd [] = return $ BoolConst True
    evalAnd [x] = evalExpr x
    evalAnd (x:xs) = do
        xEval <- evalExpr x
        case xEval of
            BoolConst False -> return xEval
            _ -> evalAnd xs
    in
        evalAnd exprs

evalExpr (Or exprs) = let
    evalOr [] = return $ BoolConst False
    evalOr [x] = evalExpr x
    evalOr (x:xs) = do
        xEval <- evalExpr x
        case xEval of
            BoolConst False -> evalOr xs
            _ -> return xEval
    in
        evalOr exprs

evalExpr symbol@(Symbol _) = return symbol

evalExpr (Pair _ _) = throwE $ SchemeError "Cannot evaluate raw pair"

evalExpr Nil = return Nil
