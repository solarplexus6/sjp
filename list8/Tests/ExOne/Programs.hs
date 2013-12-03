module Tests.ExOne.Programs where

import CommonAbsSyntax
import ExOne.AbsSyntax
import ExOne.Common (TermResult (..), OmegaResult)

withoutLoopProg = "x" := N 12 :.: 
                  If (V "x" :<=: N 20) 
                     ("x" := V "x" :+: N 11 :.: 
                      "y" := V "x" :*: N 2) 
                     Skip

withoutLoopProgResult :: OmegaResult
withoutLoopProgResult = ([], Term [("x", 23), ("y", 46)])

--

whileProg = "a" := N 20 :.: While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)

whileProgResult :: OmegaResult
whileProgResult = ([], Term [("a", 160)])

--

factorialProg = "a" := N 10 :.: "fac" := N 1 :.: While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)

factorialProgResult :: OmegaResult
factorialProgResult = ([], Term [("a", 0), ("fac", 3628800 :: Int)])

abortProg = "x" := N 1 :.:
            While (B True)
                (If (V "x" :<=: N 10)
                    ("x" := V "x" :+: N 1)
                    (Fail)
                )

abortProgResult :: OmegaResult
abortProgResult = ([], Abort [("x", 11)])

outputProg = "x" := N 1 :.:
            While (V "x" :<=: N 5)
                (Out (V "x" :*: N 2) :.: "x" := V "x" :+: N 1)

outputProgResult :: OmegaResult
outputProgResult = ([2, 4, 6, 8, 10], Term [("x", 6)])

inputProg = "x" := N 1 :.:
            While (V "x" :<=: N 15)
                (In "y" :.: "x" := V "x" :+: V "y" :.: Out (V "x"))

inputProgInput :: [Int]                
inputProgInput = [3, 5, 7]
inputProgResult :: OmegaResult                
inputProgResult = ([4, 9, 16],  Term [("x", 16), ("y", 7)])