module Common
( Epsilon
, assign
, aexp
, bexp) where

import Prelude hiding (Num)
import CommonAbsSyntax

type Epsilon = [(Var, Num)]

assign :: Var -> Num -> Epsilon -> Epsilon
assign v n [] = [(v, n)]
assign v n (f@(v', _):r) | v == v'   = (v, n):r
                         | otherwise = f : (assign v n r)

aexp :: (Aexp, Epsilon) -> Num
aexp (N n, _) = n
aexp (V v, s) = case lookup v s of
                       Nothing -> error ("unbound variable `" ++ v ++ "'")
                       Just n  -> n

aexp (a1 :+: a2, s) = aexp (a1, s) + aexp (a2, s)
aexp (a1 :-: a2, s) = aexp (a1, s) - aexp (a2, s)
aexp (a1 :*: a2, s) = aexp (a1, s) * aexp (a2, s)

bexp :: (Bexp, Epsilon) -> Bool
bexp (B b, _)           = b
bexp (a1 :=: a2, s)     = aexp (a1, s) == aexp (a2, s)
bexp (a1 :<=: a2, s)    = aexp (a1, s) <= aexp (a2, s)
bexp (Not b, s)         = not $ bexp (b, s)
bexp (b1 :&&: b2, s)    = bexp (b1, s) && bexp (b2, s)
bexp (b1 :||: b2, s)    = bexp (b1, s) || bexp (b2, s)