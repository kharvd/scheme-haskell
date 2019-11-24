module Interpreter where

import Data.Maybe
import Data.Foldable
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import qualified Control.Scanl as SL

import System.IO.Unsafe
import qualified Debug.Trace

import Ast
import Predefs
import Utils

type Scope = Map.Map Name Expr
data Environment = Environment Scope (Maybe Environment) deriving (Show)

type InterpreterState = ExceptT SchemeError (StateT Environment IO)

initEnv :: Environment
initEnv = Environment (Map.fromList predefScope) Nothing

mapScope :: (Scope -> Scope) -> Environment -> Environment
mapScope f env@(Environment scope parent) = Environment (f scope) parent

lookupScope :: Name -> Environment -> Maybe Expr
lookupScope name (Environment scope _) = Map.lookup name scope

runBody :: Body -> InterpreterState Expr
runBody = fmap last . mapM evalForm

resolve :: Name -> Environment -> Maybe Expr
resolve name (Environment scope maybeParent) =
    recoverWith (scope Map.!? name) fallback
    where fallback = maybeParent >>= resolve name

define :: Name -> Expr -> InterpreterState ()
define name value = modify $ mapScope (Map.insert name value)

replace :: Name -> Expr -> InterpreterState ()
replace name value = do
    Environment scope maybeParent <- get
    case (scope Map.!? name, maybeParent) of
        (Just z, maybeParent) -> do
            put $ Environment (Map.insert name value scope) maybeParent
            return ()
        (Nothing, Just parent) -> do
            put parent
            replace name value
            newParent <- get
            put $ Environment scope (Just newParent)
            return ()
        (Nothing, Nothing) -> throwE $ SchemeError (name ++ " is not in scope")

evalForm :: Form -> InterpreterState Expr
evalForm (ExprForm expr) = evalExpr expr
evalForm (DefForm def) = evalDef def

evalDef :: Definition -> InterpreterState Expr
evalDef (VarDef name expr) = do
    evaledExpr <- evalExpr expr
    define name evaledExpr
    return $ Var name

evalDef (FunDef name args body) = do
    define name (Lambda args body)
    return $ Var name

applyLambda :: [Name] -> Body -> [Expr] -> InterpreterState Expr
applyLambda argNames body args = do
    outerEnv <- get
    let lambdaScope = Map.fromList $ zip argNames args
    put $ Environment lambdaScope (Just outerEnv)

    result <- runBody body

    Environment _ updatedOuterEnv <- get
    put $ fromJust updatedOuterEnv

    return result


applyPredefFun :: NamedFunction -> [Expr] -> InterpreterState Expr
applyPredefFun (NamedFunction _ fun) args = ExceptT $ return (fun args)

evalExpr :: Expr -> InterpreterState Expr

evalExpr None = return None

evalExpr const@(BoolConst _) = return const
evalExpr const@(IntConst _) = return const

evalExpr (Var name) = do
    env <- get
    case (resolve name env) of
        Just x -> return x
        _ -> throwE $ SchemeError (name ++ " is undefined")

evalExpr (Set name expr) = do
    evaledExpr <- evalExpr expr
    replace name evaledExpr
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
