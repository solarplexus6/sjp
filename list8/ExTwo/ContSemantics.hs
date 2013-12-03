module ExTwo.ContSemantics
( comm
)
where

import Common
import ExTwo.AbsSyntax
import ExTwo.Common
import Data.Function (fix)

comm :: Com -> (Epsilon -> Omega) -> (Label -> Epsilon -> Omega) -> Epsilon -> Omega
comm c kt kf s = case c of
    v := a      -> kt $ assign v (aexp (a, s)) s
    Skip        -> kt s
    c1 :.: c2   -> comm c1 (comm c2 kt kf) kf s
    If b c1 c2  -> if bexp (b, s) then comm c1 kt kf s else comm c2 kt kf s
    While b c'  -> fix gamma $ s where
                       gamma w = \s' -> if bexp (b, s') then (comm c' w kf s') else kt s'
    Fail l      -> kf l s
    Catch l c1 c2   -> comm c1 kt (\l' s' -> if l' == l then (comm c2 kt kf s') else (kf l' s')) s