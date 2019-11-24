module Expression where

import Control.Monad.Trans.Except

newtype SchemeError = SchemeError String deriving (Show)
type Result = Either SchemeError

type Name = String

type Program = [Form] 
data Form = DefForm Definition | ExprForm Expr deriving (Show)

data Definition = VarDef Name Expr | FunDef Name [Name] Body deriving (Show)

type Body = [Form]

data NamedFunction = NamedFunction Name ([Expr] -> Result Expr)
instance Show NamedFunction where
    show (NamedFunction name _) = "#[" ++ name ++ "]"

data Expr 
    = BoolConst Bool
    | IntConst Integer
    | Var Name
    | Quote Expr
    | Lambda [Name] Body
    | Set Name Expr
    | And [Expr]
    | Or [Expr]
    | If Expr Expr (Maybe Expr)
    | Application Expr [Expr]
    -- Quoted only
    | Symbol Name
    | Pair Expr Expr
    | Nil
    -- Nothing
    | None
    | Predef NamedFunction
    deriving (Show)

