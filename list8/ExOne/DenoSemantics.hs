module ExOne.DenoSemantics
( aexp
, bexp
, Omega (..)
, comm)
where

import Prelude hiding (Num)
import Common
import ExOne.AbsSyntax
import Data.Function (fix)

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

data Omega = TauTerm Epsilon 
           | TauAbort Epsilon 
           | TauOut (Num, Omega)
           deriving (Eq, Show)

asterisk :: (Epsilon -> Omega) -> (Omega -> Omega)
asterisk f = \omega -> case omega of                 
                (TauTerm s)          -> f s
                (TauAbort _)         -> omega
                (TauOut (n, omega')) -> TauOut (n, asterisk f omega')
                _                    -> undefined

comm :: Com -> Epsilon -> Omega
comm c s = case c of
    v := a      -> TauTerm $ assign v (aexp (a, s)) s
    Skip        -> TauTerm s
    c1 :.: c2   -> (asterisk (comm c2)) . (comm c1) $ s
    If b c1 c2  -> if bexp (b, s) then comm c1 s else comm c2 s
    While b c'  -> fix gamma $ s where
                       gamma f = \s' -> if bexp (b, s') then (asterisk f) . (comm c') $ s' else TauTerm s'
    Fail        -> TauAbort s
    Out a       -> TauOut (aexp (a, s), TauTerm s)
    _ -> undefined