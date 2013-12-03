module Tests.ExTwo.ContSemantics where

import Test.HUnit

import ExTwo.AbsSyntax
import ExTwo.Common
import ExTwo.ContSemantics
import Tests.ExTwo.Programs

testComm :: Com -> Omega -> Assertion
testComm c expectedRes = (comm c (\s -> TauTerm s) (\l s -> TauAbort (l, s)) []) @?= expectedRes

testLoopless :: Assertion
testLoopless = testComm withoutLoopProg withoutLoopProgResult

testWhile :: Assertion
testWhile = testComm whileProg whileProgResult

testFactorial :: Assertion
testFactorial = testComm factorialProg factorialProgResult

testAbort :: Assertion
testAbort = testComm abortProg abortProgResult

testSimpleCatch :: Assertion
testSimpleCatch = testComm simpleCatchProg simpleCatchProgResult

testUnhandled :: Assertion
testUnhandled = testComm unhandledProg unhandledProgResult

testNested :: Assertion
testNested = testComm nestedCatchProg nestedCatchProgResult

testlabelClash :: Assertion
testlabelClash = testComm labelClashProg labelClashProgResult