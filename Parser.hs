module Parser  where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Ast

number :: Parser Constant
number = do
    n <- integer
    return $ (NumberConst n)

boolean :: Parser Constant
boolean = 
    let
        booleanTrue = reserved "#t" >> (return BoolTrue)
        booleanFalse = reserved "#f" >> (return BoolFalse)
    in do
        val <- try booleanTrue <|> try booleanFalse
        return $ BoolConst val

constant :: Parser Constant
constant = try number <|> try boolean

symbol :: Parser Symbol
symbol = do
    name <- identifier
    return $ Identifier name

datum :: Parser Datum
datum =
    let
        constDatum = fmap ConstDatum constant
        symbolDatum = fmap SymbolDatum symbol
        listDatum = fmap ListDatum list
    in
        try constDatum <|> try symbolDatum <|> try listDatum

_toCons :: [Datum] -> List
_toCons xs = foldr Cons EmptyList xs

list :: Parser List
list = do
    elements <- parens $ many datum
    return $ _toCons elements

variable :: Parser Expr
variable = do
    name <- identifier
    return $ Var name

quote :: Parser Expr
quote = 
    let
        datumQuote = datum >>= (return . Quote)
        quoteParens = parens (reserved "quote" >> datumQuote)
        quoteShorthand = quotation >> datumQuote
    in 
        try quoteParens <|> try quoteShorthand

body :: Parser Body
body = do
    defs <- many definition
    exprs <- many expression
    return $ (map DefForm defs) ++(map ExprForm exprs)

lambda :: Parser Expr
lambda = parens $ do
    reserved "lambda"
    vars <- parens $ many identifier
    b <- body
    return $ Lambda vars b

set :: Parser Expr
set = parens $ do
    reserved "set!"
    name <- identifier
    body <- expression
    return $ Set name body

ifExpr :: Parser Expr
ifExpr = parens $ do
    reserved "if"
    condition <- expression
    ifTrue <- expression
    ifFalse <- optionMaybe expression
    return $ If condition ifTrue ifFalse

application :: Parser Expr
application = parens $ do
    left <- expression
    right <- many expression
    return $ Application left right

expression :: Parser Expr
expression = 
    let
        constExpr = fmap Const constant
    in
        try constExpr
        <|> try variable
        <|> try quote
        <|> try lambda
        <|> try set
        <|> try ifExpr
        <|> application

functionDef :: Parser Definition
functionDef = parens $ do
    reserved "define"
    (name, args) <- parens $ do
        name <- identifier
        args <- many identifier
        return (name, args)
    b <- body
    return $ FunDef name args b

variableDef :: Parser Definition
variableDef = parens $ do
    reserved "define"
    name <- identifier
    value <- expression
    return $ VarDef name value

definition :: Parser Definition
definition = try functionDef <|> try variableDef

form :: Parser Form
form = 
    let 
        definitionForm = fmap DefForm definition
        expressionForm = fmap ExprForm expression
    in
        try definitionForm <|> expressionForm
    
program :: Parser Program
program = do
    forms <- many form
    return forms

parseProgram :: String -> Either ParseError Program
parseProgram s = parse program "<stdin>" s