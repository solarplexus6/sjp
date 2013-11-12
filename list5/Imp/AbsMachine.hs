module Imp.AbsMachine
( run
, exec
, Com (..)
, StackElem(..)
)
where

import Prelude hiding (Num)

import Imp.Common
import Imp.AbsSyntax (Num, Var)

data StackElem = N Num | B Bool
type Stack = [StackElem]

data Com = Push Num
         | Add
         | Mult
         | Sub
         | ComBool Bool
         | Eq
         | Le
         | And
         | Or
         | Not
         | Fetch Var
         | Store Var
         | Noop
         | Branch ([Com], [Com])
         | Loop ([Com], [Com])

exec :: ([Com], Stack, Store) -> ([Com], Stack, Store)
exec (Push n : c, e, s)             = (c, N n : e, s)
exec (Add : c, N n1 : N n2 : e, s)  = (c, N (n1 + n2) : e, s)
exec (Sub : c, N n1 : N n2 : e, s)  = (c, N (n1 - n2) : e, s)
exec (Mult : c, N n1 : N n2 : e, s) = (c, N (n1 * n2) : e, s)
exec (ComBool b : c, e, s)          = (c, B b : e, s)
exec (Eq : c, N n1 : N n2 : e, s)   = (c, B (n1 == n2) : e, s)
exec (Le : c, N n1 : N n2 : e, s)   = (c, B (n1 <= n2) : e, s)
exec (And : c, B b1 : B b2 : e, s)  = (c, B (b1 && b2) : e, s)
exec (Or : c, B b1 : B b2 : e, s)   = (c, B (b1 || b2) : e, s)
exec (Not : c, B b : e, s)          = (c, B (not b) : e, s)
exec (Fetch v : c, e, s)            = case lookup v s of
                                        Nothing -> error ("unbound variable `" ++ v ++ "'")
                                        Just n  -> (c, N n : e, s)
exec (Store v : c, N n : e, s)      = (c, e, assign v n s)
exec (Noop : c, e, s)               = (c, e, s)
exec (Branch (c1, c2):c, B b : e, s)= (if b then c1 else c2 ++ c, e, s)
exec (l @ (Loop (c1, c2)) : c, e, s)  = (c1 ++ (Branch (c2 ++ [l], [Noop])) : c, e, s)

exec' :: ([Com], Stack, Store) -> ([Com], Stack, Store)
exec' ([], e, s) = ([], e, s)
exec' conf = exec' $ exec conf

run :: [Com] -> Store
run c = s where (_, _, s) = exec' (c, [], [])