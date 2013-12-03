module Tests.ExTwo.Programs where

import CommonAbsSyntax
import ExTwo.AbsSyntax
import ExTwo.Common (Omega (..))

withoutLoopProg = "x" := N 12 :.: 
                  If (V "x" :<=: N 20) 
                     ("x" := V "x" :+: N 11 :.: 
                      "y" := V "x" :*: N 2) 
                     Skip

withoutLoopProgResult :: Omega
withoutLoopProgResult = TauTerm [("x", 23), ("y", 46)]

--

whileProg = "a" := N 20 :.: While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)

whileProgResult :: Omega
whileProgResult = TauTerm [("a", 160)]

--

factorialProg = "a" := N 10 :.: "fac" := N 1 :.: While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)

factorialProgResult :: Omega
factorialProgResult = TauTerm [("a", 0), ("fac", 3628800 :: Int)]

abortProg = "x" := N 1 :.:
            While (B True)
                (If (V "x" :<=: N 10)
                    ("x" := V "x" :+: N 1)
                    (Fail "Exception")
                )

abortProgResult :: Omega
abortProgResult = TauAbort ("Exception", [("x", 11)])

simpleCatchProg = "x" := N 10 :.:
                  Catch "Exception"
                      (While (B True)
                        (If (N 0 :<=: V "x")
                            ("x" := V "x" :-: N 1)
                            (Fail "Exception")
                        ))
                      ("x" := N 20)

simpleCatchProgResult :: Omega
simpleCatchProgResult = TauTerm [("x", 20)]

unhandledProg = "x" := N 0 :.:
                Catch "ExceptionTwo"
                    (If (N 0 :=: V "x")
                        (Fail "ExceptionOne")
                        ("x" := V "x" :-: N 1))
                    Skip

unhandledProgResult :: Omega
unhandledProgResult = TauAbort ("ExceptionOne", [("x", 0)])

nestedCatchProg :: Com
nestedCatchProg = "x" := N 0 :.:
                  Catch "OuterException"
                    (While (V "x" :<=: N 5)
                           (Catch "InnerException"
                                 ("x" := V "x" :-: N 1 :.:
                                  If (V "x" :<=: N 0)
                                     (Fail "OuterException")
                                     Skip)
                                 Skip))
                    ("y" := V "x" :+: N 1)

nestedCatchProgResult :: Omega
nestedCatchProgResult = TauTerm [("x", -1), ("y", 0)]

labelClashProg :: Com
labelClashProg = Catch "Exception"
                    ((Catch "Exception"
                           ("x" := N 50)
                           ("x" := N 100)) :.:
                     Fail "Exception")
                    ("x" := N 0)

labelClashProgResult :: Omega
labelClashProgResult = TauTerm [("x", 0)]