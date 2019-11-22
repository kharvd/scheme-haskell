module Ast where

type Name = String

type Program = [Form] 
data Form = DefForm Definition | ExprForm Expr deriving (Eq, Show)

data Definition = VarDef Name Expr | FunDef Name [Name] Body deriving (Eq, Show)

type Body = [Form]

data Expr 
    = Const Constant
    | Var Name
    | Quote Datum
    | Lambda [Name] Body
    | Set Name Expr
    -- | And [Expr]
    | If Expr Expr (Maybe Expr)
    | Application Expr [Expr]
    | PairExpr (Expr, Expr)
    | Nil
    | None
    deriving (Eq, Show)

data Constant = BoolConst BoolVal | NumberConst Integer deriving (Eq, Show)
data BoolVal = BoolTrue | BoolFalse deriving (Eq, Show)

data Datum = ConstDatum Constant | SymbolDatum Symbol | ListDatum List deriving (Eq, Show)
data Symbol = Identifier String deriving (Eq, Show)

data List = EmptyList | Cons Datum List deriving (Eq, Show)

    
