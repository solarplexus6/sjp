module Proc.Common
( Sigma
, assign
, lookUp) where

import Proc.AbsSyntax

type Sigma =  [(Var, Numeral)]

assign :: Var -> Numeral -> Sigma -> Sigma
assign v n [] = [(v, n)]
assign v n (f@(v', _):r) | v == v'   = (v, n):r
                         | otherwise = f : (assign v n r)

lookUp :: [Char] -> [([Char], b)] -> b
lookUp v s = 
    case lookup v s of
        Nothing -> error ("unbound variable `" ++ v ++ "'")
        Just n  -> n