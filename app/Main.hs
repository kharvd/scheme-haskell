import Repl

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

main :: IO ()
main = runRepl
