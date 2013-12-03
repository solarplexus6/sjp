module Common
( Epsilon
, Num
, Var
, assign) where

import Prelude hiding (Num)

type Num = Int
type Var = String
type Epsilon = [(Var, Num)]

assign :: Var -> Num -> Epsilon -> Epsilon
assign v n [] = [(v, n)]
assign v n (f@(v', _):r) | v == v'   = (v, n):r
                         | otherwise = f : (assign v n r)