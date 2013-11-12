module Tests.OpSemantics where

import Test.HUnit

import Imp.AbsSyntax
import Imp.OpSemantics

import Tests.Programs

testVar :: Assertion
testVar = evalA (V "x", s) @?= (N 5, s)
    where s = [("x", 5)]

testStepA :: Assertion
testStepA = evalA (N 5 :+: N 4 :+: N 3, []) @?= (N 9 :+: N 3, [])

testOrderA :: Assertion
testOrderA = evalA (N 5 :+: N 4 :*: N 3, []) @?= (N 5 :+: N 12, [])

testTransClosureA :: Assertion
testTransClosureA = evalA' (N 5 :+: N 4 :*: N 3, []) @?= (N 17, [])

testOr :: Assertion
testOr = evalB' (N 5 :<=: N 10 :||: N 12 :=: N 15, []) @?= (B True, [])

testAnd :: Assertion
testAnd = evalB' (N 5 :=: N 10 :&&: N 12 :<=: N 15, []) @?= (B False, [])

testComplexBexp :: Assertion
testComplexBexp = evalB' (N 5 :=: N 10 :&&: N 12 :<=: N 15 :||: N 1 :<=: N 2 :&&: N 3 :=: N 3, []) @?= (B True, [])

testWhile :: Assertion
testWhile = run (whileProg) @?= (Skip, whileProgFinalStore)

testFactorial :: Assertion
testFactorial = run (factorialProg) @?= (Skip, factorialProgFinalStore)
