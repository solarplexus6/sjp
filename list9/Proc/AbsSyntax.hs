module Proc.AbsSyntax where

type Numeral = Int
type Ident = String

infixl 6 :+:, :-:
infixl 7 :*:
infix  5 :=:, :<=:
infixr 4 :&&:
infixr 3 :||:
infix  2 :=
infixl 1 :~:
infixl 1 :.:

data Aexp = N Numeral       -- constant
          | V Ident         -- variable
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

data Com = Ident := Aexp                  -- assignment
         | Skip
         | Com :.: Com                  -- sequence
         | If Bexp Com Com              -- if then else
         | While Bexp Com               -- while
         | Begin Dec Com                -- blocks
         | Call Ident Aexp
         deriving (Eq, Show)

data Dec = Empty 
         | Var Ident Aexp 
         | Proc Ident Ident Com
         | Dec :~: Dec
         deriving (Eq, Show)