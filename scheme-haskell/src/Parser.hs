module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Expression
import Lexer

number :: Parser Expr
number = IntConst <$> integer

boolean :: Parser Expr
boolean =
  let booleanTrue = reserved "#t" >> return True
      booleanFalse = reserved "#f" >> return False
   in do val <- try booleanTrue <|> try booleanFalse
         return $ BoolConst val

constant :: Parser Expr
constant = try number <|> try boolean

symbol :: Parser Expr
symbol = Symbol <$> identifierOrReserved

datum :: Parser Expr
datum = try constant <|> try symbol <|> try quotedList <|> try quote

collectPair :: [Expr] -> Expr -> Expr
collectPair xs z = foldr Pair z xs

pair :: Parser Expr -> Parser Expr
pair subexpression =
  parens $ do
    elements <- many subexpression
    maybeDot <- optionMaybe dot
    collectPair elements <$>
      case maybeDot of
        Just () -> subexpression
        Nothing -> return Nil

quotedList :: Parser Expr
quotedList = pair datum

variable :: Parser Expr
variable = Var <$> identifier

quote :: Parser Expr
quote =
  let datumQuote = Quote <$> datum
      quoteParens = parens (reserved "quote" >> datumQuote)
      quoteShorthand = quotation >> datumQuote
   in try quoteParens <|> try quoteShorthand

body :: Parser Body
body = do
  defs <- many definition
  exprs <- many expression
  return $ map DefForm defs ++ map ExprForm exprs

lambda :: Parser Expr
lambda =
  parens $ do
    reserved "lambda"
    vars <- parens $ many identifier
    Lambda vars <$> body

set :: Parser Expr
set =
  parens $ do
    reserved "set!"
    name <- identifier
    Set name <$> expression

ifExpr :: Parser Expr
ifExpr =
  parens $ do
    reserved "if"
    condition <- expression
    ifTrue <- expression
    ifFalse <- optionMaybe expression
    return $ If condition ifTrue ifFalse

andExpr :: Parser Expr
andExpr =
  parens $ do
    reserved "and"
    exprs <- many expression
    return $ And exprs

orExpr :: Parser Expr
orExpr =
  parens $ do
    reserved "or"
    exprs <- many expression
    return $ Or exprs

list :: Parser Expr
list = pair expression

expression :: Parser Expr
expression = try constant <|> try variable <|> try quote <|> try lambda <|> try set <|> try ifExpr <|> list

functionDef :: Parser Definition
functionDef =
  parens $ do
    reserved "define"
    (name, args) <-
      parens $ do
        name <- identifier
        args <- many identifier
        return (name, args)
    FunDef name args <$> body

variableDef :: Parser Definition
variableDef =
  parens $ do
    reserved "define"
    name <- identifier
    VarDef name <$> expression

definition :: Parser Definition
definition = try functionDef <|> try variableDef

form :: Parser Form
form =
  let definitionForm = fmap DefForm definition
      expressionForm = fmap ExprForm expression
   in try definitionForm <|> expressionForm

program :: Parser Program
program = many form

parseProgram :: String -> Either ParseError Program
parseProgram = parse program "<stdin>"

parseForm :: String -> Either ParseError Form
parseForm = parse (Tok.whiteSpace lexer >> form) "<stdin>"