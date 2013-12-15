{-# LANGUAGE ExistentialQuantification #-}

module Tests.DenoSemantics where

import Test.HUnit hiding (State)
import Test.QuickCheck

import qualified Data.Map as M (empty, fromList, findMax)
import Control.Monad.State (runState)

import Proc.Common (zipDomain, MemAssoc)
import Proc.AbsSyntax
import Proc.DenoSemantics
import Tests.Programs

runTest semFun expr (envV, sto) = 
    let initS = case sto of
                    [] -> (M.empty, (M.empty, 0))
                    _  -> let stoMap = M.fromList sto in 
                        (M.fromList envV, (stoMap, (fst $ M.findMax stoMap) + 1))
    in
        runState (semFun expr) initS

testVar :: Assertion
testVar = fst (runTest aexp (V "x") ([("x", 0)], [(0, 5)])) @?= 5

instance Arbitrary Aexp where
    arbitrary = do
        n <- choose (minBound :: Int, maxBound :: Int) :: Gen Int
        m <- choose (minBound :: Int, maxBound :: Int) :: Gen Int
        oneof [ return (N n :+: N m)
              , return (N n :*: N m)
              , return (N n :-: N m)]

testSimpleAexp :: Aexp -> Bool
testSimpleAexp expr = testResult == expected where
    testResult = fst $ runTest aexp expr ([], [])
    expected = 
        case expr of
            (N n :+: N m) -> n + m
            (N n :-: N m) -> n - m
            (N n :*: N m) -> n * m
            _             -> error "Bad expression type"

testComplexAexp :: Assertion
testComplexAexp = fst (runTest aexp (V "x" :+: V "y" :*: N 3) ([("x", 0), ("y", 1)], [(0, 5), (1, 4)])) @?= 17

--

testComplexBexp :: Assertion
testComplexBexp = fst (runTest bexp (V "x" :+: V "y" :*: N 3 :<=: N 18 :&&: V "x" :=: N 5) ([("x", 0), ("y", 1)], [(0, 5), (1, 4)])) @?= True

--

testDecl :: Dec -> MemAssoc -> Assertion
testDecl d expectedRes = 
    let testResult = snd $ runTest decl d ([], []) in
        zipDomain testResult @?= M.fromList expectedRes

testSimpleVarDecl :: Assertion
testSimpleVarDecl = testDecl (Var "x" (N 10 :+: N 11)) [("x", 21)]

testMultipleVarDecl :: Assertion
testMultipleVarDecl = testDecl (Var "x" (N 10) :~: Var "y" (V "x" :*: N 3)) [("x", 10), ("y", 30)]

--

testComm :: Com -> MemAssoc -> Assertion
testComm c expectedRes = 
    let (IotaTerm testResult) = fst $ runTest comm c ([], []) in
        zipDomain testResult @?= M.fromList expectedRes

testLoopless :: Assertion
testLoopless = testComm withoutLoopProg withoutLoopProgResult

testWhile :: Assertion
testWhile = testComm whileProg whileProgResult

testFactorial :: Assertion
testFactorial = testComm factorialProg factorialProgResult

testDeclOverwrite :: Assertion
testDeclOverwrite = testComm (Begin (Var "x" (N 5) :~: Var "y" (N 0) :~: Var "z" (N 0))
                                (Begin (Var "x" (N 23) :~: Var "y" (V "x" :*: N 4))
                                    ("z" := V "x" :+: V "y") :.:
                                 "y" := V "x"
                                )
                             )
                            [("x", 5), ("y", 5), ("z", 115)]

-- Nielsons Exercise 6.4
testDeclRestore :: Assertion
testDeclRestore = testComm declRestoreProgram declRestoreProgramResult