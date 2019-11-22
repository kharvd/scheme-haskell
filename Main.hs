import System.Environment (getArgs)
import Parser
-- import Compiler
import Interpreter
import Printer
import Control.Monad.State.Lazy

-- main :: IO ()
-- main = do
--     args <- getArgs
--     program <- readFile $ head args
--     let parseRes = parseProgram program
--     let compiled = either (const "error") compileProgram parseRes
--     predefs <- readFile "predefs.js"
--     writeFile (args!!1) (predefs ++ "\n" ++ compiled)

main :: IO ()
main = do
    args <- getArgs
    program <- readFile $ head args
    let parseRes = parseProgram program
    let runRes = either (const []) (\res -> evalState (runProgram res) initEnv) parseRes
    putStrLn $ unlines $ map printExpr runRes
    