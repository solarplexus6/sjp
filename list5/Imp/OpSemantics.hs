module Imp.OpSemantics
( run
, exec
, exec'
, evalA
, evalA'
, evalB
, evalB'
)
where

import Imp.Common
import Imp.AbsSyntax

evalA :: (Aexp, Store) -> (Aexp, Store)
evalA ex@(N _, _) = ex
evalA (V v, s) = case lookup v s of
                       Nothing -> error ("unbound variable `" ++ v ++ "'")
                       Just n  -> (N n, s)

evalA (N n :+: N m, s) = (N (n + m), s)
evalA (N n :-: N m, s) = (N (n - m), s)
evalA (N n :*: N m, s) = (N (n * m), s)

evalA (N n :+: a2, s) = (N n :+: a2', s')
                        where (a2', s') = evalA (a2, s)
evalA (N n :-: a2, s) = (N n :-: a2', s') 
                        where (a2', s') = evalA (a2, s)
evalA (N n :*: a2, s) = (N n :*: a2', s') 
                        where (a2', s') = evalA (a2, s)

evalA (a1 :+: a2, s) = (a1' :+: a2, s')
                        where (a1', s') = evalA (a1, s)
evalA (a1 :-: a2, s) = (a1' :-: a2, s')
                        where (a1', s') = evalA (a1, s)
evalA (a1 :*: a2, s) = (a1' :*: a2, s')
                        where (a1', s') = evalA (a1, s)

evalA' :: (Aexp, Store) -> (Aexp, Store)
evalA' c@(N _, _) = c
evalA' c          = evalA' $ evalA c

evalB :: (Bexp, Store) -> (Bexp, Store)
evalB (bExp, s) = 
    case bExp of B b       -> (B b, s)
                 Not (B b) -> (B (not b), s)                 
                 Not b     -> (Not b', s') where
                                  (b', s') = evalB (b, s)
                 N n :=: N m    -> (B (n == m), s)
                 N n :=: a2     -> (N n :=: a2', s') where
                                           (a2', s') = evalA (a2, s)
                 a1  :=: a2     -> (a1' :=: a2, s') where
                                           (a1', s') = evalA (a1, s)
                 N n :<=: N m   -> (B (n <= m), s)
                 N n :<=: a2    -> (N n :<=: a2', s') where
                                            (a2', s') = evalA (a2, s)
                 a1  :<=: a2    -> (a1' :<=: a2, s') where
                                            (a1', s') = evalA (a1, s)
                 B b1 :||: B b2 -> (B (b1 || b2), s)
                 B b1 :||: b2   -> (B b1 :||: b2', s') where
                                             (b2', s') = evalB (b2, s)
                 b1  :||: b2    -> (b1' :||: b2, s') where
                                            (b1', s') = evalB (b1, s)
                 B b1 :&&: B b2 -> (B (b1 && b2), s)
                 B b1 :&&: b2   -> (B b1 :&&: b2', s') where
                                             (b2', s') = evalB (b2, s)
                 b1  :&&: b2    -> (b1' :&&: b2, s') where
                                            (b1', s') = evalB (b1, s)

evalB' :: (Bexp, Store) -> (Bexp, Store)
evalB' c@(bExp, _) = 
    case bExp of B _    -> c
                 _ -> evalB' $ evalB c

exec :: (Com, Store) -> (Com, Store)
exec (com, s) =
    case com of v := N n        -> (Skip, assign v n s)
                v := a          -> (v := a', s')
                                        where (a', s') = evalA (a, s)
                Skip            -> (Skip, s)
                Skip :.: c2     -> (c2, s)
                c1 :.: c2       -> (c1' :.: c2, s') where
                                        (c1', s') = exec (c1, s)
                If (B b) c1 c2  -> if b then (c1, s) else (c2, s)
                If b c1 c2      -> (If b' c1 c2, s') where
                                        (b', s') = evalB (b, s)
                While b c       -> (If b (c :.: While b c) Skip, s)

exec' :: (Com, Store) -> (Com, Store)
exec' c@(Skip, _) = c
exec' c           = exec' $ exec c

run :: Com -> (Com, Store)
run c = exec' (c, [])