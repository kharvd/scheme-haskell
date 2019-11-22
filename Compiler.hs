module Compiler where

import Data.List (intercalate)
import Ast

mangleName :: String -> String
mangleName name = 
    let 
        mangleChar '+' = "_plus_"
        mangleChar '-' = "_minus_"
        mangleChar '.' = "_dot_"
        mangleChar '!' = "_excl_"
        mangleChar '%' = "_percent_"
        mangleChar '&' = "_amp_"
        mangleChar '*' = "_star_"
        mangleChar '/' = "_fwdslash_"
        mangleChar ':' = "_colon_"
        mangleChar '<' = "_lt_"
        mangleChar '=' = "_eq_"
        mangleChar '>' = "_gt_"
        mangleChar '?' = "_q_"
        mangleChar '~' = "_tilde_"
        mangleChar '^' = "_caret_"
        mangleChar c = [c]
    in 
        name >>= mangleChar

compileConstant :: Constant -> String
compileConstant (BoolConst value) = 
    let
        compileBool BoolTrue = "true"
        compileBool _ = "false"
    in
        compileBool value
compileConstant (NumberConst value) = show value

compileSymbol :: Symbol -> String
compileSymbol (Identifier name) = "\"" ++ name ++ "\""

compileList :: List -> String
compileList Nil = "[]"
compileList (Cons head tail) = "[" ++ (compileDatum head) ++ ", " ++ (compileList tail) ++ "]"

compileDatum :: Datum -> String
compileDatum (ConstDatum const) = compileConstant const
compileDatum (SymbolDatum symbol) = compileSymbol symbol
compileDatum (ListDatum list) = compileList list

compileBody :: Body -> String
compileBody (defs, exprs) = 
    (intercalate ";\n" $ map compileDef defs) ++ ";\n" ++
        (intercalate ";\n" $ map compileExpr (init exprs)) ++ ";\n" ++
        "return (" ++ (compileExpr $ last exprs) ++ ");";

compileExpr :: Expr -> String
compileExpr (Const const) = compileConstant const
compileExpr (Var name) = mangleName name
compileExpr (Quote datum) = compileDatum datum
compileExpr (Lambda args body) = 
    "(" ++ (intercalate "," $ map mangleName args) ++ ") => {\n" ++ (compileBody body) ++ "\n}";

compileExpr (Set name expr) = name ++ " = " ++ (compileExpr expr)
compileExpr (If cond ifTrue maybeIfFalse) = 
    let 
        compileMaybeElse Nothing = "\n"
        compileMaybeElse (Just ifFalse) = "else {\n" ++ "return (" ++ (compileExpr ifFalse) ++ ");\n" ++ "}\n"
    in "(() => {if (" ++ (compileExpr cond) ++ ") {" ++ "\n" ++
            "return (" ++ (compileExpr ifTrue) ++ ");\n" ++
            "}" ++ (compileMaybeElse maybeIfFalse) ++ "})()"

compileExpr (Application expr args) = (compileExpr expr) ++ "(" ++ (intercalate "," $ map compileExpr args) ++ ")"


compileDef :: Definition -> String
compileDef (VarDef name expr) = "let " ++ (mangleName name) ++ " = (" ++ (compileExpr expr) ++ ")"
compileDef (FunDef name args body) = "let " ++ (mangleName name) ++ " = (" ++ (compileExpr $ Lambda args body) ++ ")"

compileForm :: Form -> String
compileForm (DefForm def) = compileDef def
compileForm (ExprForm expr) = compileExpr expr

compileProgram :: Program -> String
compileProgram program = (intercalate ";\n" $ map compileForm program)