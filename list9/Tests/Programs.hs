module Tests.Programs where

import Proc.AbsSyntax
import Proc.DenoSemantics

withoutLoopProg :: Com
withoutLoopProg = "x" := N 12 :.:
                  If (V "x" :<=: N 20)
                     ("x" := V "x" :+: N 11 :.:
                      "y" := V "x" :*: N 2)
                     Skip

withoutLoopProgResult :: Omega
withoutLoopProgResult = IotaTerm [("x", 23), ("y", 46)]

--

whileProg :: Com
whileProg = "a" := N 20 :.: While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)

whileProgResult :: Omega
whileProgResult = IotaTerm [("a", 160)]

--

factorialProg :: Com
factorialProg = "a" := N 10 :.: "fac" := N 1 :.: While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)

factorialProgResult :: Omega
factorialProgResult = IotaTerm [("a", 0), ("fac", 3628800 :: Int)]