module Interpreter where

import Data.Maybe
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import System.IO.Unsafe
import qualified Debug.Trace

import Ast
import Predefs

-- trace :: String -> a -> a
-- trace string expr = unsafePerformIO $ do
--     Debug.Trace.traceIO string
--     return expr

type Scope = Map.Map Name Expr
data Environment = Environment Scope (Maybe Environment) deriving (Show)

mapScope :: (Scope -> Scope) -> Environment -> Environment
mapScope f env@(Environment scope parent) = Environment (f scope) parent

lookupScope :: Name -> Environment -> Maybe Expr
lookupScope name (Environment scope _) = Map.lookup name scope

initEnv :: Environment
initEnv = Environment (Map.fromList predefScope) Nothing

runProgram :: Program -> State Environment [Expr]
runProgram = mapM evalForm

runBody :: Body -> State Environment Expr
runBody = fmap last . mapM evalForm

recoverWith :: Maybe a -> Maybe a -> Maybe a
recoverWith m1@(Just x) _ = m1
recoverWith _ m2 = m2

resolve :: Name -> Environment -> Maybe Expr
resolve name (Environment scope maybeParent) =
    recoverWith (scope Map.!? name) fallback
    where fallback = maybeParent >>= (resolve name)

define :: Name -> Expr -> State Environment ()
define name value = modify $ mapScope (Map.insert name value)

replace :: Name -> Expr -> State Environment ()
replace name value = do
    Environment scope maybeParent <- get
    put $ case (scope Map.!? name, maybeParent) of
        ((Just z), maybeParent) -> Environment (Map.insert name value scope) maybeParent
        (Nothing, (Just parent)) -> Environment scope (Just $ execState (replace name value) parent)
        (Nothing, Nothing) -> error $ name ++ " is undefined"
    return ()

evalForm :: Form -> State Environment Expr
evalForm (ExprForm expr) = evalExpr expr
evalForm (DefForm def) = evalDef def

evalDef :: Definition -> State Environment Expr
evalDef (VarDef name expr) = do
    evaledExpr <- evalExpr expr
    define name evaledExpr
    return $ Var name

evalDef (FunDef name args body) = do
    define name (Lambda args body)
    return $ Var name

applyLambda :: [Name] -> Body -> [Expr] -> State Environment Expr
applyLambda argNames body args = do
    outerEnv <- get
    let lambdaScope = Map.fromList $ zip argNames args
    put $ Environment lambdaScope (Just outerEnv)

    result <- runBody body

    Environment _ updatedOuterEnv <- get
    put $ fromJust updatedOuterEnv

    return result

applyPredefFun :: NamedFunction -> [Expr] -> State Environment Expr
applyPredefFun (NamedFunction _ fun) args = return $ fun args

evalExpr :: Expr -> State Environment Expr

evalExpr None = return None

evalExpr const@(BoolConst _) = return const
evalExpr const@(IntConst _) = return const

evalExpr (Var name) = do
    env <- get
    case (resolve name env) of
        Just x -> return x
        _ -> error $ name ++ " is undefined"

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
        _ -> error "Not applicable"

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

evalExpr (Pair _ _) = error "Cannot evaluate raw pair"

evalExpr Nil = return Nil
