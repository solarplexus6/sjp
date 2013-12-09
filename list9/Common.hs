module Common
( Epsilon
, assign) where

import Block.AbsSyntax

type Epsilon = [(Var, Numeral)]

assign :: Var -> Numeral -> Epsilon -> Epsilon
assign v n [] = [(v, n)]
assign v n (f@(v', _):r) | v == v'   = (v, n):r
                         | otherwise = f : (assign v n r)