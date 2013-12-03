module CommonAbsSyntax where

import Prelude hiding (Num)

type Num = Int
type Var = String

infixl 6 :+:, :-:
infixl 7 :*:
infix  5 :=:, :<=:
infixr 4 :&&:
infixr 3 :||:

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