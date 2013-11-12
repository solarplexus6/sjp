module Imp.AbsSyntax
( Num
, Var
, Aexp (..)
, Bexp (..)
, Com (..)
) where

import Prelude hiding (Num)

infixl 6 :+:, :-:
infixl 7 :*:
infix  5 :=:, :<=:
infixr 4 :&&:
infixr 3 :||:
infix  2 :=
infixl  1 :.:

type Num = Int
type Var = String

data Aexp = N Num           -- constant
          | V Var           -- variable
          | Aexp :+: Aexp   -- addition
          | Aexp :-: Aexp   -- subtraction
          | Aexp :*: Aexp   -- multiplication
          deriving (Eq, Show)

data Bexp = B Bool
          | Aexp :=: Aexp
          | Aexp :<=: Aexp 
          | Not Bexp
          | Bexp :&&: Bexp
          | Bexp :||: Bexp
          deriving (Eq, Show)

data Com = Var := Aexp                  -- assignment
         | Skip
         | Com :.: Com                  -- sequence
         | If Bexp Com Com              -- if then else
         | While Bexp Com               -- while
         deriving (Eq, Show)