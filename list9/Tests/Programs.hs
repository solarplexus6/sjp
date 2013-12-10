module Tests.Programs where

import Proc.Common (MemAssoc)
import Proc.AbsSyntax

withoutLoopProg :: Com
withoutLoopProg = "x" := N 12 :.:
                  If (V "x" :<=: N 20)
                     ("x" := V "x" :+: N 11 :.:
                      "y" := V "x" :*: N 2)
                     Skip

withoutLoopProgResult :: MemAssoc
withoutLoopProgResult = [("x", 23), ("y", 46)]

--

whileProg :: Com
whileProg = "a" := N 20 :.: While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)

whileProgResult :: MemAssoc
whileProgResult = [("a", 160)]

--

factorialProg :: Com
factorialProg = "a" := N 10 :.: "fac" := N 1 :.: While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)

factorialProgResult :: MemAssoc
factorialProgResult = [("a", 0), ("fac", 3628800)]

declRestoreProgram :: Com
declRestoreProgram = Begin (Var "y" (N 0) :~: 
                            Var "x" (N 1))
                        (
                            Begin (Var "x" (N 7)) ("x" := V "x" :+: N 1) :.:
                            "y" := V "x"
                        )

declRestoreProgramResult :: MemAssoc
declRestoreProgramResult = [("x", 1), ("y", 1)]
