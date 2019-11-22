module Interpreter where

import Data.Maybe
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import System.IO.Unsafe
import qualified Debug.Trace

import Ast

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
initEnv = Environment Map.empty Nothing

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

    let (argNames, body) = case evaledFun of
            Lambda argNames body -> (argNames, body)
            _ -> error "Not applicable"
        lambdaScope = Map.fromList $ zip argNames evaledArgs

    outerEnv <- get
    put $ Environment lambdaScope (Just outerEnv)
    result <- runBody body
    Environment _ updatedOuterEnv <- get

    put $ fromJust updatedOuterEnv
    return result

evalExpr (Quote expr) = return expr 


-- datumExpr :: Datum -> Expr
-- datumExpr (ConstDatum const) = Const const
-- datumExpr (SymbolDatum (Identifier name)) = Var name
-- datumExpr (ListDatum )