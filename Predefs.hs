module Predefs (predefScope) where

import Ast

predefScope :: [(Name, Expr)]
predefScope = map toPair predefs
    where 
        toPair :: NamedFunction -> (Name, Expr)
        toPair nf@(NamedFunction name _) = (name, Predef nf)

predefs :: [NamedFunction]
predefs = [ 
    NamedFunction "+" add 
    ]

add :: [Expr] -> Expr
add = IntConst . add' where
    add' :: [Expr] -> Integer
    add' [] = 0
    add' ((IntConst n):xs) = n + (add' xs)
    add' _ = error "not a number"