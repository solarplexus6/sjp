module Main
where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup, Test)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tests.Eager.DenoSemantics as Eager
import Tests.Lazy.DenoSemantics as Lazy

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
    testGroup "Eager variant tests" [
            testGroup "Arithmetic expressions" [
                testCase "Base ops with let" Eager.testLet1
              ],
            testGroup "Boolean expressions" [
                testCase "Equality, literals, not equal" Eager.testEq1
              ],
            testGroup "Expressions" [
                testCase "Snd type error with lambda" Eager.testSndTypeError
              , testCase "Pair, lambda, boolean operators" Eager.testPairLambda
              , testCase "Alternative" Eager.testAlt
              , testCase "Factorial" Eager.testFactorial
              ],
            testGroup "Eager evaluation" [
              -- if jest leniwy ("short-circuit") we wszystkich gorliwych jezykach funkcyjnych jakie znam, wiec tak to tez zostawilem
              testCase "If, infinite recursion" Eager.testInfiniteRecIf
              -- ponizszy test poprawnie konczy sie przez stack overflow
            --, testCase "Pair, Application, infinite recursion" Eager.testInfiniteRecPair
            -- tutaj rowniez wszystko dzieje sie gorliwie i zapetla sie
            --, testCase "Application, let, infinite recursion" Eager.testInfiniteRecLetApp
            ]
          ],
    testGroup "Lazy variant tests" [
            testGroup "Arithmetic expressions" [
                testCase "Base ops with let" Lazy.testLet1
              ],
            testGroup "Boolean expressions" [
                testCase "Equality, literals, not equal" Lazy.testEq1
              ],
            testGroup "Expressions" [
                testCase "Snd type error with lambda" Lazy.testSndTypeError
              , testCase "Pair, lambda, boolean operators" Lazy.testPairLambda
              , testCase "Alternative" Lazy.testAlt
              , testCase "Factorial" Lazy.testFactorial
              ],
            testGroup "Lazy evaluation" [
              testCase "If, infinite recursion" Lazy.testInfiniteRecIf
            , testCase "Pair, Application, infinite recursion" Lazy.testInfiniteRecPair
            , testCase "Infinite list" Lazy.testInfiniteList
            ]
          ]
    ]