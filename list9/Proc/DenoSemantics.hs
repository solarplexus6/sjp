{-# LANGUAGE ExistentialQuantification #-}

module Proc.DenoSemantics
( comm
, bexp
, aexp
, decl
)
where

import Control.Monad.State (get, put)
import Data.Function (fix)

import Proc.Common
import Proc.AbsSyntax

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

comm :: Com -> Sigma Omega
comm c = case c of
    v := a     -> do
                    d  <- get
                    ar <- aexp a
                    newD <- return $ assign v ar d
                    put $ newD
                    return $ IotaTerm newD
    Skip       -> get >>= (return . IotaTerm)
    c1 :.: c2  -> do
                    D (envV, envP, _) <- get -- wyciagamy pierwotne srodowisko
                    comm c1          -- wywolujemy c1
                    D (_, _, newS) <- get -- olewamy zmienione srodowisko, wyciagamy nowy store
                    put $ D (envV, envP, newS) -- przywracamy stare srodowisko
                    comm c2          -- wywolujemy c2
                    
    If b c1 c2 -> bexp b >>= \br -> if br then comm c1 else comm c2    
    While b c' -> fix gamma where
                      gamma f = bexp b >>= \br -> if br then (comm c') >> f else comm Skip
    --Begin d c' -> decl d >> comm c' >> get >>= (return . IotaTerm)
    Begin d c' -> decl d >> comm c'
    -- ze wzgledu na uzycie State Monad i przyjetego w zwiazku z tym modelu
    -- Call sie komplikuje w stosunku do oryginalnej definicji z NN07, ale 
    -- za to cala reszta implementacji prawie sie nie zmienia
    Call p     -> do
                    D (envV, envP, sto) <- get -- pierwotne srodowisko
                    let (pEnvV, pEnvP, runProc) = lookUpProc p envP -- wyciagamy envV, envP dla procedury oraz monade runProc reprezentujaca wykonanie p
                    put $ D (pEnvV, pEnvP, sto)
                    runProc
                    D (_, _, newS) <- get
                    put $ D (envV, envP, newS)
                    get >>= (return . IotaTerm)

decl :: Dec -> (Sigma ())
decl d = case d of
    Var v a -> do
        n   <- aexp a
        dom <- get
        put (newVar v n dom)
    --
    d1 :~: d2 -> decl d1 >> decl d2
    --
    Proc p c -> do
        dom@(D (envV, envP, _)) <- get        
        let gamma g = do 
            D (_, _, sto) <- get -- wyciagamy obecne sto
            put (newProc p g $ D (envV, envP, sto)) -- update envP, ale tak zeby nie zepsuc sto
            comm c -- wykonanie wywolania rek.
        put (newProc p (fix gamma) dom)

