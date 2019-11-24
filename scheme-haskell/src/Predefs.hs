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
add = NamedFunction "+" $ fmap IntConst . add' where
    add' :: [Expr] -> Result Integer
    add' [] = Right 0
    add' (IntConst n : xs) = fmap (+ n) $ add' xs
    add' _ = Left $ SchemeError "not a number"

car :: NamedFunction
car = NamedFunction "car" car'
car' [Pair head _] = Right head
car' [x] = Left $ SchemeError "not a pair"
car' _ = Left $ SchemeError "incorrect number of arguments"

cdr :: NamedFunction
cdr = NamedFunction "cdr" cdr'
cdr' [Pair _ tail] = Right tail
cdr' [x] = Left $ SchemeError "not a pair"
cdr' _ = Left $ SchemeError "incorrect number of arguments"