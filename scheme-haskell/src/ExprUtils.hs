module ExprUtils where

import Ast

isList :: Expr -> Bool
isList Nil = True
isList (Pair _ cdr) = isList cdr
isList _ = False
