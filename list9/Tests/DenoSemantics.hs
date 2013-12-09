module Tests.DenoSemantics where

import Test.HUnit hiding (State)
import Test.QuickCheck

import Control.Monad.State (State, runState)

import Proc.AbsSyntax
import Proc.DenoSemantics
import Tests.Programs

runTest :: (t -> State b a) -> t -> b -> a
runTest semFun expr initState = fst $ runState (semFun expr) initState

testVar :: Assertion
testVar = runTest aexp (V "x") [("x", 5)] @?= 5

instance Arbitrary Aexp where
    arbitrary = do
        n <- choose (minBound :: Int, maxBound :: Int) :: Gen Int
        m <- choose (minBound :: Int, maxBound :: Int) :: Gen Int
        oneof [ return (N n :+: N m)
              , return (N n :*: N m)
              , return (N n :-: N m)]

testSimpleAexp :: Aexp -> Bool
testSimpleAexp expr = testResult == expected where
    testResult = runTest aexp expr []
    expected = 
        case expr of
            (N n :+: N m) -> n + m
            (N n :-: N m) -> n - m
            (N n :*: N m) -> n * m
            otherwise     -> error "Bad expression type"

testComplexAexp :: Assertion
testComplexAexp = runTest aexp (V "x" :+: V "y" :*: N 3) [("x", 5), ("y", 4)] @?= 17

--

testComplexBexp :: Assertion
testComplexBexp = runTest bexp (V "x" :+: V "y" :*: N 3 :<=: N 18 :&&: V "x" :=: N 5) [("x", 5), ("y", 4)] @?= True

--

testComm :: Com -> Omega -> Assertion
testComm c expectedRes = runTest comm c [] @?= expectedRes

testLoopless :: Assertion
testLoopless = testComm withoutLoopProg withoutLoopProgResult

testWhile :: Assertion
testWhile = testComm whileProg whileProgResult

testFactorial :: Assertion
testFactorial = testComm factorialProg factorialProgResult