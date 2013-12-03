module ExTwo.AbsSyntax
( Com (..)
, Label
) where

import CommonAbsSyntax

infix  2 :=
infixl  1 :.:

type Label = String

data Com = Var := Aexp                  -- assignment
         | Skip
         | Com :.: Com                  -- sequence
         | If Bexp Com Com              -- if then else
         | While Bexp Com               -- while
         | Fail Label
         | Catch Label Com Com          -- try/catch
         deriving (Eq, Show)