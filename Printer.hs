module Printer where

import Data.List (intercalate)
import Data.Maybe (maybeToList)

import Ast
import ExprUtils

    -- = BoolConst Bool
    -- | IntConst Integer
    -- | Var Name
    -- | Quote Expr
    -- | Lambda [Name] Body
    -- | Set Name Expr
    -- | And [Expr]
    -- | Or [Expr]
    -- | If Expr Expr (Maybe Expr)
    -- | Application Expr [Expr]
    -- -- Quoted only
    -- | Symbol Name
    -- | Pair Expr Expr
    -- | Nil
    -- -- Nothing
    -- | None
    -- | Predef NamedFunction
printForm :: Form -> String
printForm (ExprForm expr) = printExpr expr

printSpaced :: [String] -> String
printSpaced = intercalate " "

printBody :: Body -> String
printBody body = printSpaced $ map printForm body

printList :: [Expr] -> String
printList body = "(" ++ (printSpaced $ map printExpr body) ++ ")"

printPair :: [Expr] -> Expr -> String
printPair init last = "(" ++ (printSpaced $ map printExpr init) ++ " . " ++ (printExpr last) ++ ")"

printExpr :: Expr -> String
printExpr (BoolConst True) = "#t"
printExpr (BoolConst False) = "#f"
printExpr (IntConst n) = show n 
printExpr (Var name) = name
printExpr (Quote expr) = "'" ++ printExpr expr
printExpr (Lambda args body) = "(lambda (" ++ (printSpaced args) ++ ") " ++ (printBody body) ++ ")"
printExpr (Set name value) = printList [Var "set!", Var name, value]
printExpr (And exprs) = printList exprs 
printExpr (Or exprs) = printList exprs
printExpr (If cond ifTrue maybeIfFalse) = printList ([Var "if", cond, ifTrue] ++ maybeToList maybeIfFalse)
printExpr (Application fun body) = printList $ [fun] ++ body
printExpr (Symbol name) = name
printExpr pair@(Pair car cdr) = 
    let
        extractPair :: Expr -> ([Expr], Expr)
        extractPair Nil = ([], Nil)
        extractPair (Pair car cdr) = 
            let (init, last) = extractPair cdr
            in (car:init, last)

        (init, last) = extractPair pair
    in case last of
        Nil -> printList init
        _ -> printPair init last
printExpr Nil = "()"
printExpr None = ""
printExpr (Predef fun) = show fun


