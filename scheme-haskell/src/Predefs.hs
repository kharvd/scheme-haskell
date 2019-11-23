module Predefs (predefScope) where

import Control.Monad.Trans.Except

import Ast

predefScope :: [(Name, Expr)]
predefScope = map toPair predefs
    where 
        toPair :: NamedFunction -> (Name, Expr)
        toPair nf@(NamedFunction name _) = (name, Predef nf)

predefs :: [NamedFunction]
predefs = 
    [ 
      add 
    , car 
    , cdr 
    ]

add :: NamedFunction
add = NamedFunction "+" $ return . IntConst . add' where
    add' :: [Expr] -> Integer
    add' [] = 0
    add' (IntConst n : xs) = n + add' xs
    add' _ = error "not a number"

car :: NamedFunction
car = NamedFunction "car" car'
car' [Pair head _] = return head
car' [x] = throwE $ SchemeError "not a pair"
car' _ = throwE $ SchemeError "incorrect number of arguments"

cdr :: NamedFunction
cdr = NamedFunction "cdr" cdr'
cdr' [Pair _ tail] = return tail
cdr' [x] = throwE $ SchemeError "not a pair"
cdr' _ = throwE $ SchemeError "incorrect number of arguments"