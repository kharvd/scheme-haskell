module Repl
  ( runRepl
  ) where

import Control.Monad (void)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT(..), catchE, runExceptT)
import System.Console.Haskeline
import System.Environment (getArgs)

import Expression
import Interpreter
import Parser
import Printer

import Text.Parsec (ParseError)

type InterpreterIO = InterpreterT IO

instance (MonadException m) => MonadException (ExceptT e m) where
  controlIO f =
    ExceptT $
    controlIO $ \(RunIO run) ->
      let run' = RunIO (fmap ExceptT . run . runExceptT)
       in runExceptT <$> f run'

handleError :: SchemeError -> InterpreterIO Expr
handleError (SchemeError msg) = do
  liftIO $ putStrLn msg
  return None

process :: String -> InterpreterIO ()
process line =
  case parseForm line of
    Left err -> liftIO $ print err
    Right form -> do
      expr <- catchE (evalForm form) handleError
      liftIO $ putStrLn $ printExpr expr

runRepl :: IO ()
runRepl = void $ runInterpreterT (runInputT defaultSettings loop) initEnv
  where
    loop :: InputT InterpreterIO ()
    loop = do
      minput <- getInputLine "scheme> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> lift (process input) >> loop