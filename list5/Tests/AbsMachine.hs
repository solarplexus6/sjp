module Tests.AbsMachine where

import Test.HUnit

import Imp.AbsSyntax
import Imp.AbsMachine

import Tests.Programs

testWhile :: Assertion
testWhile = run (whileAbsProg) @?= whileProgFinalStore

testFactorial :: Assertion
testFactorial = run (factorialAbsProg) @?= factorialProgFinalStore