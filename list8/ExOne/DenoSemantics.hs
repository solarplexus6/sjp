module ExOne.DenoSemantics
( comm
)
where

import Prelude hiding (Num)
import Common
import ExOne.Common
import ExOne.AbsSyntax
import Data.Function (fix)

asterisk :: (Epsilon -> Omega) -> (Omega -> Omega)
asterisk f = \omega -> case omega of
                (TauTerm s)          -> f s
                (TauAbort _)         -> omega
                (TauOut (n, omega')) -> TauOut (n, asterisk f omega')
                (TauIn g)            -> TauIn (\k -> asterisk f (g k))
                _                    -> undefined

comm :: Com -> Epsilon -> Omega
comm c s = case c of
    v := a      -> TauTerm $ assign v (aexp (a, s)) s
    Skip        -> TauTerm s
    c1 :.: c2   -> (asterisk $ comm c2) . (comm c1) $ s
    If b c1 c2  -> if bexp (b, s) then comm c1 s else comm c2 s
    While b c'  -> fix gamma s where
                       gamma f = \s' -> if bexp (b, s') then (asterisk f) . (comm c') $ s' else TauTerm s'
    Fail        -> TauAbort s
    Out a       -> TauOut (aexp (a, s), TauTerm s)
    In v        -> TauIn (\k -> TauTerm (assign v k s))