module Tests.DenoSemantics where

import Test.HUnit
import Test.QuickCheck

import Proc.AbsSyntax
import Proc.DenoSemantics
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

testComm :: Com -> Omega -> Assertion
testComm c expectedRes = (comm c []) @?= expectedRes

testLoopless :: Assertion
testLoopless = testComm withoutLoopProg withoutLoopProgResult

testWhile :: Assertion
testWhile = testComm whileProg whileProgResult

testFactorial :: Assertion
testFactorial = testComm factorialProg factorialProgResult