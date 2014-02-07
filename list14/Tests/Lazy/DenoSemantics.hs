{-# LANGUAGE ExistentialQuantification #-}

module Tests.Lazy.DenoSemantics where

import Test.HUnit hiding (State)
-- import Test.QuickCheck

import qualified Data.Map as M (empty)

import Tests.Lazy.Programs
import Fun.Lazy.AbsSyntax
import Fun.Lazy.Domain
import Fun.Lazy.DenoSemantics

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

testInfiniteList :: Assertion
testInfiniteList = runTest infiniteList infiniteListResult