module Lexer where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

reservedNames = ["define", "quote", "lambda", "set!", "if", "#t", "#f"]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    identStart = letter <|> oneOf "!$%&*/:<=>?~_^"
    identLetter = identStart <|> alphaNum <|> oneOf "+-."
    style = emptyDef {
               Tok.commentLine = ";"
             , Tok.reservedNames = reservedNames
             , Tok.identStart = identStart
             , Tok.identLetter = identLetter
             }

natural :: Parser Integer
natural = Tok.natural lexer

integer :: Parser Integer
integer = do
    plusMinus <- optionMaybe $ oneOf "+-"
    let mult = case plusMinus of Just '+' -> 1 
                                 Nothing -> 1
                                 Just '-' -> -1
    num <- natural
    return $ mult * num

parens :: Parser a -> Parser a
parens = Tok.parens lexer

plusOrMinus :: Parser String
plusOrMinus = do
    x <- oneOf "+-"
    Tok.whiteSpace lexer
    return [x]

identifier :: Parser String
identifier = plusOrMinus <|> (Tok.identifier lexer)

reserved :: String -> Parser String
reserved name = Tok.reserved lexer name >> return name

identifierOrReserved :: Parser String
identifierOrReserved = try identifier <|> (choice $ map reserved reservedNames)

quotation :: Parser ()
quotation = fmap (\x -> ()) $ char '\''