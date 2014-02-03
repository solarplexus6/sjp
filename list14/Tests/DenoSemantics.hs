{-# LANGUAGE ExistentialQuantification #-}

module Tests.DenoSemantics where

import Test.HUnit hiding (State)
-- import Test.QuickCheck

import qualified Data.Map as M (empty)

import Tests.Common.Programs
import Fun.AbsSyntax
import Fun.Common
import Fun.Strict.DenoSemantics

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

testFactorial :: Assertion
testFactorial = sem factorialProg M.empty @?= factorialProgResult