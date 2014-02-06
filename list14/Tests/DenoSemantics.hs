{-# LANGUAGE ExistentialQuantification #-}

module Tests.DenoSemantics where

import Test.HUnit hiding (State)
-- import Test.QuickCheck

import qualified Data.Map as M (empty)

import Tests.Common.Programs
import Fun.AbsSyntax
import Fun.Common
import Fun.Eager.DenoSemantics

runTest :: Exp -> V' -> Assertion
runTest prog results = sem prog M.empty @?= results

testEq1 :: Assertion
testEq1 = runTest eqProg1 eqProg1Result

testLet1 :: Assertion
testLet1 = runTest letProg1 letProg1Result

testPairLambda :: Assertion
testPairLambda = runTest pairAndLambda pairAndLambdaResult

testSndTypeError :: Assertion
testSndTypeError = runTest typeErrorSnd typeErrorSndResult

testAlt :: Assertion
testAlt = runTest alt altResult

testFactorial :: Assertion
testFactorial = runTest factorialProg factorialProgResult

testInfiniteRecPair :: Assertion
testInfiniteRecPair = runTest infiniteRecPair infiniteRecPairResult

testInfiniteRecIf :: Assertion
testInfiniteRecIf = runTest infiniteRecIf infiniteRecIfResult