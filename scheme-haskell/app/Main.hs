import System.Environment (getArgs)
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Trans.Except
import System.Console.Haskeline

import Parser
-- import Compiler
import Interpreter
import Printer
import Ast

import Text.Parsec (ParseError)

-- main :: IO ()
-- main = do
--     args <- getArgs
--     program <- readFile $ head args
--     let parseRes = parseProgram program
--     let compiled = either (const "error") compileProgram parseRes
--     predefs <- readFile "predefs.js"
--     writeFile (args!!1) (predefs ++ "\n" ++ compiled)

-- printRes :: Result Expr -> String
-- printRes (Left (SchemeError msg)) = msg
-- printRes (Right exprs) = printExpr exprs

-- handleError :: ParseError -> Result Expr
-- handleError error = Left $ SchemeError $ "parse error: \n" ++ show error

-- main :: IO ()
-- main = do
--     args <- getArgs
--     program <- readFile $ head args
--     let parseRes = parseProgram program
--     let runRes = either (\error -> [handleError error]) runProgram parseRes
--     putStrLn $ unlines $ map printRes runRes

-- instance MonadException InterpreterState where
--     -- controlIO :: (RunIO InterpreterState -> IO (InterpreterState a)) -> InterpreterState a
--     controlIO f = 

instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

handleError :: SchemeError -> InterpreterState Expr
handleError (SchemeError msg) = do
    liftIO $ putStrLn msg
    return None

process :: String -> InterpreterState ()
process line = 
    case parseForm line of
        Left err -> liftIO $ print err
        Right form -> do
            expr <- catchE (evalForm form) handleError
            liftIO $ putStrLn $ printExpr expr

main :: IO ()
main = void $ runStateT (runExceptT (runInputT defaultSettings loop)) initEnv
  where
  loop :: InputT InterpreterState ()
  loop = do
    minput <- getInputLine "scheme> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> lift (process input) >> loop