module Predefs (predefScope) where

import Ast

predefScope :: [(Name, Expr)]
predefScope = map toPair predefs
    where 
        toPair :: NamedFunction -> (Name, Expr)
        toPair nf@(NamedFunction name _) = (name, Predef nf)

predefs :: [NamedFunction]
predefs = 
    [ 
      NamedFunction "+" add 
    , NamedFunction "car" car 
    , NamedFunction "cdr" cdr 
    ]

add :: [Expr] -> Expr
add = IntConst . add' where
    add' :: [Expr] -> Integer
    add' [] = 0
    add' ((IntConst n):xs) = n + (add' xs)
    add' _ = error "not a number"

car :: [Expr] -> Expr
car [(Pair head _)] = head
car [x] = error "not a pair"
car _ = error "incorrect number of arguments"

cdr :: [Expr] -> Expr
cdr [(Pair _ tail)] = tail
cdr [x] = error "not a pair"
cdr _ = error "incorrect number of arguments"