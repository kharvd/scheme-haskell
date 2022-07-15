module Interpreter
  ( InterpreterT
  , runInterpreterT
  , evalForm
  , initEnv
  ) where

import Control.Monad.State.Strict (StateT, evalStateT)
import qualified Control.Monad.State.Strict as ST
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(..), catchE, except, runExceptT, throwE)
import Data.Maybe

import qualified Environment as Env
import Environment (Environment)
import Expression
import Predefs
import Utils

type InterpreterT m = ExceptT SchemeError (StateT (Environment Expr) m)

runInterpreterT :: (Monad m) => InterpreterT m a -> Environment Expr -> m (Result a)
runInterpreterT computation = evalStateT (runExceptT computation)

initEnv :: Environment Expr
initEnv = Env.create predefScope Nothing

evalForm :: (Monad m) => Form -> InterpreterT m Expr
evalForm (ExprForm expr) = evalExpr expr
evalForm (DefForm def) = evalDef def

defineVar :: (Monad m) => Name -> Expr -> InterpreterT m Expr
defineVar name expr = do
  evaledExpr <- evalExpr expr
  ST.modify $ Env.insert name evaledExpr
  return $ Var name

defineFun :: (Monad m) => Name -> [Name] -> Body -> InterpreterT m Expr
defineFun name args body = do
  ST.modify $ Env.insert name (Lambda args body)
  return $ Var name

evalDef :: (Monad m) => Definition -> InterpreterT m Expr
evalDef (VarDef name expr) = defineVar name expr
evalDef (FunDef name args body) = defineFun name args body

withEnv :: (Monad m) => Environment Expr -> InterpreterT m Expr -> InterpreterT m Expr
withEnv env computation = do
  oldEnv <- ST.get
  let handleError e = do
        ST.put oldEnv
        throwE e
  ST.put env
  result <- catchE computation handleError
  innerEnv <- ST.get
  case Env.parent innerEnv of
    Just x -> ST.put x
    Nothing -> error "fatal error: empty parent environment"
  return result

checkNumArgs :: (Monad m) => Int -> Int -> InterpreterT m ()
checkNumArgs expected actual
  | expected > actual = throwE $ SchemeError "too few args"
checkNumArgs expected actual
  | expected < actual = throwE $ SchemeError "too many args"
checkNumArgs expected actual = return ()

runBody :: (Monad m) => Body -> InterpreterT m Expr
runBody = fmap last . mapM evalForm

applyLambda :: (Monad m) => [Name] -> Body -> [Expr] -> InterpreterT m Expr
applyLambda argNames body args = do
  checkNumArgs (length argNames) (length args)
  outerEnv <- ST.get
  let lambdaEnv = Env.create (zip argNames args) (Just outerEnv)
  withEnv lambdaEnv (runBody body)

applyPredefFun :: (Monad m) => NamedFunction -> [Expr] -> InterpreterT m Expr
applyPredefFun (NamedFunction _ fun) args = ExceptT $ return (fun args)

evalVar :: (Monad m) => Name -> InterpreterT m Expr
evalVar name = do
  env <- ST.get
  case Env.resolve name env of
    Just x -> return x
    _ -> throwE $ SchemeError (name ++ " is undefined")

evalSet :: (Monad m) => Name -> Expr -> InterpreterT m Expr
evalSet name expr = do
  evaledExpr <- evalExpr expr
  env <- ST.get
  let maybeNewEnv = Env.replace name evaledExpr env
  case maybeNewEnv of
    Just newEnv -> ST.put newEnv
    Nothing -> throwE $ SchemeError (name ++ " is not in scope")
  return None

evalApplication :: (Monad m) => Expr -> [Expr] -> InterpreterT m Expr
evalApplication fun args = do
  evaledFun <- evalExpr fun
  evaledArgs <- mapM evalExpr args
  case evaledFun of
    Lambda argNames body -> applyLambda argNames body evaledArgs
    Predef fun -> applyPredefFun fun evaledArgs
    _ -> throwE $ SchemeError "Not applicable"

evalIf :: (Monad m) => Expr -> Expr -> Maybe Expr -> InterpreterT m Expr
evalIf cond ifTrue maybeIfFalse = do
  condEval <- evalExpr cond
  case (condEval, maybeIfFalse) of
    (BoolConst False, Just ifFalse) -> evalExpr ifFalse
    (BoolConst False, Nothing) -> return None
    _ -> evalExpr ifTrue

evalAnd :: (Monad m) => [Expr] -> InterpreterT m Expr
evalAnd [] = return $ BoolConst True
evalAnd [x] = evalExpr x
evalAnd (x:xs) = do
  xEval <- evalExpr x
  case xEval of
    BoolConst False -> return xEval
    _ -> evalAnd xs

evalOr :: (Monad m) => [Expr] -> InterpreterT m Expr
evalOr [] = return $ BoolConst False
evalOr [x] = evalExpr x
evalOr (x:xs) = do
  xEval <- evalExpr x
  case xEval of
    BoolConst False -> evalOr xs
    _ -> return xEval

evalPair :: (Monad m) => Expr -> Expr -> InterpreterT m Expr
evalPair car cdr =
  let extractArgs (Pair head tail) = do
        tailArgs <- extractArgs tail
        return $ head : tailArgs
      extractArgs Nil = return []
      extractArgs _ = Left $ SchemeError "cannot evaluate improper pair"
   in do args <- except $ extractArgs cdr
         evalApplication car args

evalExpr :: (Monad m) => Expr -> InterpreterT m Expr
evalExpr None = return None
evalExpr const@(BoolConst _) = return const
evalExpr const@(IntConst _) = return const
evalExpr (Var name) = evalVar name
evalExpr (Set name expr) = evalSet name expr
evalExpr lambda@(Lambda _ _) = return lambda
evalExpr (Quote expr) = return expr
evalExpr (If cond ifTrue maybeIfFalse) = evalIf cond ifTrue maybeIfFalse
evalExpr (And exprs) = evalAnd exprs
evalExpr (Or exprs) = evalOr exprs
evalExpr symbol@(Symbol _) = return symbol
evalExpr (Pair car cdr) = evalPair car cdr
evalExpr Nil = return Nil