module Tests.ExOne.ContSemantics where

import Test.HUnit

import ExOne.AbsSyntax
import ExOne.Common
import ExOne.ContSemantics
import Tests.ExOne.Programs

testComm :: Com -> [Int] -> OmegaResult -> Assertion
testComm c input result = evalOmega (comm c (\s -> TauTerm s) []) input @?= result

testLoopless :: Assertion
testLoopless = testComm withoutLoopProg [] withoutLoopProgResult

testWhile :: Assertion
testWhile = testComm whileProg [] whileProgResult

testFactorial :: Assertion
testFactorial = testComm factorialProg [] factorialProgResult

testAbort :: Assertion
testAbort = testComm abortProg [] abortProgResult

testOutput :: Assertion
testOutput = testComm outputProg [] outputProgResult

testInput :: Assertion
testInput = testComm inputProg inputProgInput inputProgResult