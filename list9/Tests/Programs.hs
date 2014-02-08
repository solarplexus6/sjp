module Tests.Programs where

import Proc.Common (MemAssoc)
import Proc.AbsSyntax

withoutLoopProg :: Com
withoutLoopProg = Begin (Var "x" (N 12) :~: Var "y" (N 0))
                  (    
                    If (V "x" :<=: N 20)
                        ("x" := V "x" :+: N 11 :.:
                         "y" := V "x" :*: N 2)
                        Skip
                  )

withoutLoopProgResult :: MemAssoc
withoutLoopProgResult = [("x", 23), ("y", 46)]

--

whileProg :: Com
whileProg = Begin (Var "a" (N 20)) 
                (
                    While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)
                )

whileProgResult :: MemAssoc
whileProgResult = [("a", 160)]

--

factorialProg :: Com
factorialProg = Begin (Var "a" (N 10) :~: Var "fac" (N 1)) 
                (
                    While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)
                )

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

staticScope1 :: Com
staticScope1 = 
    Begin ( Var "x" (N 7) :~: 
            Var "y" (N 10) :~:
            Proc "p" "()" ("x" := N 0)) $
        (
            Begin (Var "x" (N 5)) (Call "p" (N 0)) :.:
            "y" := V "x"
        )

staticScope1Result :: MemAssoc
staticScope1Result = [("x", 0), ("y", 0)]

staticScope2 :: Com
staticScope2 =
    Begin (
        Var "x" (N 0) :~: 
        Proc "p" "()" ("x" := (V "x" :+: N 1)) :~:
        Proc "q" "()" (Call "p" (N 0)))
    (
        Begin (Proc "p" "()" ("x" := N 7))
            (Call "q" (N 0))
    )

staticScope2Result :: MemAssoc
staticScope2Result = [("x", 1)]

-- wersja dla procedur bez argumentow
factorialProc :: Com
factorialProc =
    Begin (
        Var "x" (N 5) :~:
        Var "y" (N 1) :~:
        Proc "fac" "()"
            (Begin (Var "z" (V "x"))
                (If (V "x" :=: N 1)
                    Skip
                    ("x" := V "x" :-: N 1 :.: Call "fac" (N 0) :.: "y" := V "z" :*: V "y"))
            )
        )
        (Call "fac" (N 0))

factorialProcResult :: MemAssoc
factorialProcResult = [("x", 1), ("y", 120)]

procArgNonRecursive :: Com
procArgNonRecursive = 
    Begin (
        Var "y" (N 0) :~:
        Proc "test" "x"
            (If (V "x" :=: N 3)
                ("y" := V "x" :*: N 2)
                Skip)
        )
        (Call "test" (N 1 :+: N 2))

procArgNonRecursiveResult :: MemAssoc
procArgNonRecursiveResult = [("y", 6)]

procArgRecursive :: Com
procArgRecursive = 
    Begin (
        Var "y" (N 0) :~:
        Proc "test" "x"
            (If (V "x" :=: N 0)
                Skip
                ("y" := V "y" :+: N 2 :.: Call "test" (V "x" :-: N 1)))
        )
        (Call "test" (N 10))

procArgRecursiveResult :: MemAssoc
procArgRecursiveResult = [("y", 20)]