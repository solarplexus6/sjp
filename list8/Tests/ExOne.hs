module Tests.ExOne where

import Test.HUnit
import Test.QuickCheck

import ExOne.AbsSyntax
import ExOne.DenoSemantics

import Tests.Programs

testVar :: Assertion
testVar = aexp (V "x", [("x", 5)]) @?= 5

instance Arbitrary Aexp where
    arbitrary = do
        n <- choose (minBound :: Int, maxBound :: Int) :: Gen Int
        m <- choose (minBound :: Int, maxBound :: Int) :: Gen Int
        oneof [ return (N n :+: N m)
              , return (N n :*: N m)
              , return (N n :-: N m)]

testSimpleAexp :: Aexp -> Bool
testSimpleAexp expr @ (N n :+: N m) = aexp (expr, []) == (n + m)
testSimpleAexp expr @ (N n :-: N m) = aexp (expr, []) == (n - m)
testSimpleAexp expr @ (N n :*: N m) = aexp (expr, []) == (n * m)
testSimpleAexp _ = False

testComplexAexp :: Assertion
testComplexAexp = aexp (V "x" :+: V "y" :*: N 3, [("x", 5), ("y", 4)]) @?= 17

--

testComplexBexp :: Assertion
testComplexBexp = bexp (V "x" :+: V "y" :*: N 3 :<=: N 18 :&&: V "x" :=: N 5, [("x", 5), ("y", 4)]) @?= True

--

testLoopless :: Assertion
testLoopless = comm withoutLoopProg [] @?= TauTerm withoutLoopProgFinalStore

testWhile :: Assertion
testWhile = comm whileProg [] @?= TauTerm whileProgFinalStore

testFactorial :: Assertion
testFactorial = comm factorialProg [] @?= TauTerm factorialProgFinalStore

testAbort :: Assertion
testAbort = comm abortProg [] @?= TauAbort abortProgFinalStore

testOutput :: Assertion
testOutput = comm outputProg [] @?= outputProgFinalOmega