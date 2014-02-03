module Tests.Common.Programs where

import Fun.AbsSyntax
import Fun.Common

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

factorialProg :: Exp
factorialProg = Letrec "fact" "n" (If (V "n" :=: N 0) (N 1) (V "n" :*: (App (V "fact") (V "n" :-: N 1))))
                    (App (V "fact") (N 5))

factorialProgResult :: V'
factorialProgResult = Vnorm $ Vint 120