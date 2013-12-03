module Main
where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Tests.ExOne.DenoSemantics
import qualified Tests.ExOne.ContSemantics

import qualified Tests.ExTwo.DenoSemantics
import qualified Tests.ExTwo.ContSemantics

main = defaultMain tests

mainWithOpts = do
    -- Test options can also be specified in the code. The TestOptions
    -- type is an instance of the Monoid type class, so the easiest way
    -- to get an empty set of options is with `mempty`.
    let empty_test_opts = mempty :: TestOptions

    -- We update the empty TestOptions with our desired values.
    let my_test_opts = empty_test_opts {
        topt_maximum_generated_tests = Just 500
    }

    -- Now we create an empty RunnerOptions in the same way, and add
    -- our TestOptions to it.
    let empty_runner_opts = mempty :: RunnerOptions
    let my_runner_opts = empty_runner_opts {
        ropt_test_options = Just my_test_opts
    }

    defaultMainWithOpts tests my_runner_opts

tests = [
    testGroup "Fail + Input + Output" [
        testGroup "Denotational semantics tests" [
            testGroup "Arithmetic expressions" [
                testCase "Variable" Tests.ExOne.DenoSemantics.testVar,
                testProperty "Simple" Tests.ExOne.DenoSemantics.testSimpleAexp,
                testCase "Complex" Tests.ExOne.DenoSemantics.testComplexAexp
              ],
            testGroup "Boolean expressions" [
                testCase "Complex" Tests.ExOne.DenoSemantics.testComplexBexp
              ],
            testGroup "Commands" [
                  testCase "Loopless" Tests.ExOne.DenoSemantics.testLoopless
                , testCase "While" Tests.ExOne.DenoSemantics.testWhile
                , testCase "Factorial" Tests.ExOne.DenoSemantics.testFactorial
                , testCase "Abort" Tests.ExOne.DenoSemantics.testAbort
                , testCase "Output" Tests.ExOne.DenoSemantics.testOutput
                , testCase "Input" Tests.ExOne.DenoSemantics.testInput
              ]
          ],
        testGroup "Continuation semantics tests" [
            testGroup "Commands" [
                  testCase "Loopless" Tests.ExOne.ContSemantics.testLoopless
                , testCase "While" Tests.ExOne.ContSemantics.testWhile
                , testCase "Factorial" Tests.ExOne.ContSemantics.testFactorial
                , testCase "Abort" Tests.ExOne.ContSemantics.testAbort
                , testCase "Output" Tests.ExOne.ContSemantics.testOutput
                , testCase "Input" Tests.ExOne.ContSemantics.testInput
                ]
            ]
        ],
    testGroup "Try/catch" [
        testGroup "Denotational semantics tests" [
            testGroup "Commands" [
                  testCase "Loopless" Tests.ExTwo.DenoSemantics.testLoopless
                , testCase "While" Tests.ExTwo.DenoSemantics.testWhile
                , testCase "Factorial" Tests.ExTwo.DenoSemantics.testFactorial
                , testCase "Abort" Tests.ExTwo.DenoSemantics.testAbort
                , testCase "Simple Catch" Tests.ExTwo.DenoSemantics.testSimpleCatch
                , testCase "Unhandled" Tests.ExTwo.DenoSemantics.testUnhandled
                , testCase "Nested catch" Tests.ExTwo.DenoSemantics.testNested
                , testCase "Nested catch label clash" Tests.ExTwo.DenoSemantics.testlabelClash
                ]
            ],
        testGroup "Continuation semantics tests" [
            testGroup "Commands" [
                  testCase "Loopless" Tests.ExTwo.ContSemantics.testLoopless
                , testCase "While" Tests.ExTwo.ContSemantics.testWhile
                , testCase "Factorial" Tests.ExTwo.ContSemantics.testFactorial
                , testCase "Abort" Tests.ExTwo.ContSemantics.testAbort
                , testCase "Simple Catch" Tests.ExTwo.ContSemantics.testSimpleCatch
                , testCase "Unhandled" Tests.ExTwo.ContSemantics.testUnhandled
                , testCase "Nested catch" Tests.ExTwo.ContSemantics.testNested
                , testCase "Nested catch label clash" Tests.ExTwo.ContSemantics.testlabelClash
                ]
            ]
        ]
    ]