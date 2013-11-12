module Tests.Programs where

import Imp.AbsSyntax
import qualified Imp.AbsMachine as Am

whileProg = "a" := N 20 :.: While (V "a" :<=: N 100) ("a" := V "a" :*: N 2)

whileProgFinalStore = [("a", 160 :: Int)]

factorialProg = "a" := N 10 :.: "fac" := N 1 :.: While (Not $ V "a" :<=: N 0) ("fac" := V "fac" :*: V "a" :.: "a" := V "a" :-: N 1)

factorialProgFinalStore = [("a", 0), ("fac", 3628800 :: Int)]

whileAbsProg = [Am.Push 20, Am.Store "a", Am.Loop ([Am.Push 100, Am.Fetch "a", Am.Le], [Am.Push 2, Am.Fetch "a", Am.Mult, Am.Store "a"])]
whileAbsProgFinalStore = [("a", 160 :: Int)]