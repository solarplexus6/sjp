{-# LANGUAGE ExistentialQuantification #-}

module Proc.DenoSemantics
( comm
, bexp
, aexp
, Omega (..)
)
where

import Control.Monad.State
import Proc.Common
import Proc.AbsSyntax

data Omega = IotaTerm Sigma
           deriving (Eq, Show)

--liftBin :: (Monad m) => (t -> m t) -> (t -> t -> b) -> t -> t -> m b

liftBin :: Monad m => (t -> m t1) -> (t1 -> t1 -> b) -> t -> t -> m b
liftBin semFun f expr1 expr2 = do
    r1 <- semFun expr1
    r2 <- semFun expr2
    return $ r1 `f` r2

computeA :: forall b. Aexp -> (Numeral -> Numeral -> b) -> Aexp -> State Sigma b
computeA a1 op a2 = liftBin aexp op a1 a2

computeB :: Bexp -> (Bool -> Bool -> Bool) -> Bexp -> State Sigma Bool
computeB b1 op b2 = liftBin bexp op b1 b2

aexp :: Aexp -> (State Sigma Numeral)
aexp expr = case expr of
    (N n)       -> return n
    (V v)       -> do
        get >>= \s -> return $ lookUp v s
    (a1 :+: a2) -> computeA a1 (+) a2
    (a1 :-: a2) -> computeA a1 (-) a2
    (a1 :*: a2) -> computeA a1 (*) a2

bexp :: Bexp -> (State Sigma Bool)
bexp expr = case expr of
    B b        -> return b
    a1 :=: a2  -> computeA a1 (==) a2
    a1 :<=: a2 -> computeA a1 (<=) a2
    Not b      -> bexp b >>= (return . not)
    b1 :&&: b2 -> computeB b1 (&&) b2
    b1 :||: b2 -> computeB b1 (||) b2

comm :: Com -> (State Sigma Omega)
comm c = case c of
    v := a     -> 
        let declare = do 
            s  <- get
            ar <- aexp a
            return $ assign v ar s
        in do
            newS <- declare
            put $ newS
            return $ IotaTerm newS
    Skip       -> get >>= (return . IotaTerm)
    c1 :.: c2  -> comm c1 >> comm c2        
    If b c1 c2 -> bexp b >>= \br -> if br then comm c1 else comm c2    
    While b c' -> fix gamma where
                      gamma f = bexp b >>= \br -> if br then (comm c') >> f else comm Skip
    _          -> undefined