{-# LANGUAGE ExistentialQuantification #-}

module Proc.DenoSemantics
( comm
, bexp
, aexp
, decl
, Omega (..)
)
where

import Control.Monad.State

import Proc.Common
import Proc.AbsSyntax

data Omega = IotaTerm Domain
           deriving (Eq, Show)

liftBin :: Monad m => (t -> m t1) -> (t1 -> t1 -> b) -> t -> t -> m b
liftBin semFun f expr1 expr2 = do
    r1 <- semFun expr1
    r2 <- semFun expr2
    return $ r1 `f` r2

computeA :: forall b. Aexp -> (Numeral -> Numeral -> b) -> Aexp -> Sigma b
computeA a1 op a2 = liftBin aexp op a1 a2

computeB :: Bexp -> (Bool -> Bool -> Bool) -> Bexp -> Sigma Bool
computeB b1 op b2 = liftBin bexp op b1 b2

aexp :: Aexp -> (Sigma Numeral)
aexp expr = case expr of
    (N n)       -> return n
    (V v)       -> do
        get >>= \s -> return $ lookUp v s
    (a1 :+: a2) -> computeA a1 (+) a2
    (a1 :-: a2) -> computeA a1 (-) a2
    (a1 :*: a2) -> computeA a1 (*) a2

bexp :: Bexp -> (Sigma Bool)
bexp expr = case expr of
    B b        -> return b
    a1 :=: a2  -> computeA a1 (==) a2
    a1 :<=: a2 -> computeA a1 (<=) a2
    Not b      -> bexp b >>= (return . not)
    b1 :&&: b2 -> computeB b1 (&&) b2
    b1 :||: b2 -> computeB b1 (||) b2

comm :: Com -> (Sigma Omega)
comm c = case c of
    v := a     -> do
                    s  <- get
                    ar <- aexp a
                    newS <- return $ assign v ar s
                    put $ newS
                    return $ IotaTerm newS
    Skip       -> get >>= (return . IotaTerm)
    c1 :.: c2  -> comm c1 >> comm c2        
    If b c1 c2 -> bexp b >>= \br -> if br then comm c1 else comm c2    
    While b c' -> fix gamma where
                      gamma f = bexp b >>= \br -> if br then (comm c') >> f else comm Skip
    --Begin d c' -> decl d >> comm c' >> get >>= (return . IotaTerm)
    Begin d c' -> do
                    (oldEnvV, _) <- get
                    IotaTerm (newEnvV, newStore) <- decl d >> comm c'
                    put (restore oldEnvV newEnvV, newStore)
                    get >>= (return . IotaTerm)
    _          -> undefined

decl :: Dec -> (Sigma ())
decl d = case d of
    Var v a -> do
        n <- aexp a
        s <- get
        put (assign v n s)
    d1 :~: d2 -> decl d1 >> decl d2        
    _ -> undefined