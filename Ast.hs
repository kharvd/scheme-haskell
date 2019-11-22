module Ast where

type Name = String

type Program = [Form] 
data Form = DefForm Definition | ExprForm Expr deriving (Eq, Show)

data Definition = VarDef Name Expr | FunDef Name [Name] Body deriving (Eq, Show)

type Body = [Form]

data Expr 
    = BoolConst Bool
    | IntConst Integer
    | Var Name
    | Quote Expr
    | Lambda [Name] Body
    | Set Name Expr
    -- | And [Expr]
    | If Expr Expr (Maybe Expr)
    | Application Expr [Expr]
    | Symbol Name
    | Pair Expr Expr
    | Nil
    | None
    deriving (Eq, Show)

    
