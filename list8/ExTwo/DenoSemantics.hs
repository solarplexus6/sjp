module ExTwo.DenoSemantics
( comm
)
where

import Common
import ExTwo.AbsSyntax
import ExTwo.Common
import Data.Function (fix)

comm :: Com -> Epsilon -> Omega
comm c s = case c of
    v := a      -> undefined
    Skip        -> undefined
    c1 :.: c2   -> undefined
    If b c1 c2  -> undefined
    While b c'  -> undefined
    Fail l      -> undefined
    Catch l c1 c2   -> undefined