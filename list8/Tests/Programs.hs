module Tests.Programs where

import ExOne.AbsSyntax
import ExOne.DenoSemantics (Omega (..))

withoutLoopProg = "x" := N 12 :.: 
                  If (V "x" :<=: N 20) 
                     ("x" := V "x" :+: N 11 :.: 
                      "y" := V "x" :*: N 2) 
                     Skip

withoutLoopProgFinalStore :: [(String, Int)]
withoutLoopProgFinalStore = [("x", 23), ("y", 46)]

--

whileProg = "a" := N 20 :.: While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)

whileProgFinalStore :: [(String, Int)]
whileProgFinalStore = [("a", 160)]

--

factorialProg = "a" := N 10 :.: "fac" := N 1 :.: While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)

factorialProgFinalStore :: [(String, Int)]
factorialProgFinalStore = [("a", 0), ("fac", 3628800 :: Int)]

abortProg = "x" := N 1 :.:
            While (B True)
                (If (V "x" :<=: N 10)
                    ("x" := V "x" :+: N 1)
                    (Fail)
                )

abortProgFinalStore :: [(String, Int)]
abortProgFinalStore = [("x", 11)]

outputProg = "x" := N 1 :.:
            While (V "x" :<=: N 5)
                (Out (V "x" :*: N 2) :.: "x" := V "x" :+: N 1)

outputProgFinalOmega :: Omega
outputProgFinalOmega = TauOut (2, TauOut(4, TauOut(6, TauOut(8, TauOut(10, TauTerm [("x", 6)])))))