module ExTwo.Common where

import Common (Epsilon)
import ExTwo.AbsSyntax (Label)

data Omega = TauTerm Epsilon
           | TauAbort (Label, Epsilon)
           deriving (Eq, Show)