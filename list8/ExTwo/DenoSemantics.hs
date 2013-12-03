module ExTwo.DenoSemantics
( comm
)
where

import Common
import ExTwo.AbsSyntax
import ExTwo.Common
import Data.Function (fix)

asterisk :: (Epsilon -> Omega) -> (Omega -> Omega)
asterisk f = \omega -> case omega of
                (TauTerm s)          -> f s
                (TauAbort _)         -> omega
                _                    -> undefined

comm :: Com -> Epsilon -> Omega
comm c s = case c of
    v := a      -> TauTerm $ assign v (aexp (a, s)) s
    Skip        -> TauTerm s
    c1 :.: c2   -> (asterisk (comm c2)) . (comm c1) $ s
    If b c1 c2  -> if bexp (b, s) then comm c1 s else comm c2 s
    While b c'  -> fix gamma $ s where
                       gamma f = \s' -> if bexp (b, s') then (asterisk f) . (comm c') $ s' else TauTerm s'
    Fail l      -> TauAbort (l, s)
    Catch l c1 c2   -> let res = comm c1 s in
    				   case res of
    						TauAbort (l', s') -> if l == l' then comm c2 s' else res
    						_ -> res