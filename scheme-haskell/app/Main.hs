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

maybeHandleError :: Result (Expr, Environment) -> Result (Expr, Environment)
-- maybeHandleError result = 

process :: String -> InterpreterState ()
process line = 
  let res = parseForm line
  in case res of
        Left err -> liftIO $ print err
        Right form -> mapStateT maybeHandleError $ evalForm form
            -- liftIO $ putStrLn $ printExpr expr
  

main :: IO ()
main = void $ runExceptT $ runStateT (runInputT defaultSettings loop) initEnv
  where
  loop :: InputT InterpreterState ()
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> lift (process input) >> loop