module Main
where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup, Test)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tests.DenoSemantics

main :: IO ()
main = defaultMain tests

mainWithOpts :: IO ()
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

tests  :: [Test]
tests = [
    testGroup "Denotational semantics tests" [
            testGroup "Arithmetic expressions" [
                testCase "Base ops with let" Tests.DenoSemantics.testLet1
              ],
            testGroup "Boolean expressions" [
                testCase "Equality, literals, not equal" Tests.DenoSemantics.testEq1
              ],
            testGroup "Expressions" [
                testCase "Snd type error with lambda" Tests.DenoSemantics.testSndTypeError
              , testCase "Pair, lambda, boolean operators" Tests.DenoSemantics.testPairLambda
              , testCase "Factorial" Tests.DenoSemantics.testFactorial
              ]
          ]
    ]