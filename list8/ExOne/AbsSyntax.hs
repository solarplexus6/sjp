module ExOne.AbsSyntax
( Com (..)
) where

import CommonAbsSyntax

infix  2 :=
infixl  1 :.:

data Com = Var := Aexp                  -- assignment
         | Skip
         | Com :.: Com                  -- sequence
         | If Bexp Com Com              -- if then else
         | While Bexp Com               -- while
         | Fail                         -- abort
         | In Var                       -- input
         | Out Aexp                     -- output
         deriving (Eq, Show)