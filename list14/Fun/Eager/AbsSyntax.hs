module Fun.Eager.AbsSyntax where

type Numeral = Int
type Ident = String

infixl 6 :+:, :-:
infixl 7 :*:
infix  5 :=:, :<=:
infixr 4 :&&:
infixr 3 :||:
--infix  2 :=

data Exp = N Numeral
         | Exp :+: Exp   -- addition
         | Exp :-: Exp   -- subtraction
         | Exp :*: Exp   -- multiplication
         | B Bool
         | Exp :=: Exp
         | Exp :<=: Exp 
         | Not Exp
         | Exp :&&: Exp
         | Exp :||: Exp
         | V Ident
         | If Exp Exp Exp
         | Lambda Ident Exp
         | App Exp Exp
         | Let Ident Exp Exp
         | Letrec Ident Ident Exp Exp
         -- pair
         | Pair Exp Exp
         | Fst Exp
         | Snd Exp
         -- alternative
         | Inl Exp
         | Inr Exp
         | Case Exp Ident Exp Ident Exp
         deriving (Eq, Show)