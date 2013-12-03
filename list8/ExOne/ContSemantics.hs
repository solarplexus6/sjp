module ExOne.ContSemantics
( comm
)
where

import Prelude hiding (Num)
import Common
import ExOne.Common
import ExOne.AbsSyntax
import Data.Function (fix)

comm :: Com -> (Epsilon -> Omega) -> Epsilon -> Omega
comm c k s = case c of
    v := a      -> k $ assign v (aexp (a, s)) s
    Skip        -> k s
    c1 :.: c2   -> ((comm c1) . (comm c2)) k s
    If b c1 c2  -> if bexp (b, s) then comm c1 k s else comm c2 k s
    While b c'  -> fix gamma $ s where
                       gamma w = \s' -> if bexp (b, s') then (comm c' w s') else k s'
    Fail        -> TauAbort s
    Out a       -> TauOut (aexp (a, s), k s)
    In v        -> TauIn (\m -> k (assign v m s))