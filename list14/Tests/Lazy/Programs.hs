module Tests.Lazy.Programs where

import Fun.Lazy.AbsSyntax
import Fun.Lazy.Domain

eqProg1 :: Exp
eqProg1 = (N 10) :=: (N 12)

eqProg1Result :: V'
eqProg1Result = Vnorm $ Vbool False

eqProg2 :: Exp
eqProg2 = (N 102) :=: (N 102)

eqProg2Result :: V'
eqProg2Result = Vnorm $ Vbool True

letProg1 :: Exp
letProg1 = Let "v" (N 3 :*: N 4 :+: N 5) (V "v" :-: N 10)

letProg1Result :: V'
letProg1Result = Vnorm $ Vint 7

typeErrorSnd :: Exp
typeErrorSnd = Snd (Lambda "x" (V "x" :*: V "x"))

typeErrorSndResult :: V'
typeErrorSndResult = TypeError

pairAndLambda :: Exp
pairAndLambda = Fst (Pair (App (Lambda "x" (V "x" :<=: N 12 :&&: Not (N 12 :=: V "x"))) $ N 11) (N 0))

pairAndLambdaResult :: V'
pairAndLambdaResult = Vnorm $ Vbool True

alt :: Exp
alt = Let "fun" (Lambda "x" (Case (V "x") "vl" (N 2 :*: V "vl") "vr" (V "vr" :-: N 10)))
        (Pair (App (V "fun") (Inl $ N 10 :+: N 5)) (App (V "fun") (Inr $ N 256 :-: N 100)))

altResult :: V'
altResult = Vnorm $ Vpair (Vnorm $ Vint 30, Vnorm $  Vint 146)

factorialProg :: Exp
factorialProg = Letrec "fact" (Lambda "n" $ If (V "n" :=: N 0) (N 1) (V "n" :*: (App (V "fact") (V "n" :-: N 1))))
                    (App (V "fact") (N 5))

factorialProgResult :: V'
factorialProgResult = Vnorm $ Vint 120

infiniteRecPair :: Exp
infiniteRecPair = Letrec "infinite" (Lambda "n" $ V "n" :+: (App (V "infinite") (V "n" :-: N 1)))
                (Fst $ Pair (N 0) (App (V "infinite") (N 1000)))

infiniteRecPairResult :: V'
infiniteRecPairResult = Vnorm $ Vint 0

infiniteRecIf :: Exp
infiniteRecIf = Letrec "infinite" (Lambda "n" $ V "n" :+: (App (V "infinite") (V "n" :-: N 1)))
                (If (B True) (N 1) (App (V "infinite") (N 1000)))

infiniteRecIfResult :: V'
infiniteRecIfResult = Vnorm $ Vint 1

includelistLib :: Exp -> Exp
includelistLib e = 
    Let "empty" (Inr Unit)
    $ -- in
    Let "cons"
        (Lambda "x" $ Lambda "xs" $ (Inl $ Pair (V "x") (V "xs")))
    $ -- in
    Let "head" 
        (Lambda "xs" $ Case (V "xs") ":" (Fst $ V ":") "empty" Error) 
    $ -- in
    Let "tail" 
        (Lambda "xs" $ Case (V "xs") ":" (Snd $ V ":") "empty" Error)
    $ -- in
    Letrec "drop" 
        (Lambda "n" $ Lambda "xs" $ 
            If (V "n" :=: N 0) 
                (V "xs") $ 
                Case (V "xs") ":" (App (App (V "drop") (V "n" :-: N 1)) (Snd $ V ":")) "empty" (V "xs"))
    $ -- in
    Letrec "zerosOnes" (App (App (V "cons") (N 0)) (App (App (V "cons") (N 1)) (V "zerosOnes")))
    $ -- in
    e

infiniteList :: Exp
infiniteList = includelistLib $ App (V "head") (App (App (V "drop") $ N 3) $ V "zerosOnes")

infiniteListResult :: V'
infiniteListResult = Vnorm $ Vint 1