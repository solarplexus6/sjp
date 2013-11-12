module Imp.Common
( Store
, assign) where

import Imp.AbsSyntax as AbsSyntax

type Store = [(Var, AbsSyntax.Num)]

assign :: Var -> AbsSyntax.Num -> Store -> Store
assign v n [] = [(v, n)]
assign v n (f@(v', _):r) | v == v'   = (v, n):r
                       | otherwise = f : (assign v n r)