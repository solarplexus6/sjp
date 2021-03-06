module Fun.Eager.DenoSemantics
( sem
)
where

import Data.Function (fix)

import Fun.Eager.AbsSyntax
import Fun.Common
import Fun.Eager.Domain

-- lift dla inta
_int :: (Vint -> V') -> V -> V'
_int f (Vint x) = f x
_int _ _        = TypeError

_bool :: (Vbool -> V') -> V -> V'
_bool f (Vbool x) = f x
_bool _ _        = TypeError

_fun :: (Vfun -> V') -> V -> V'
_fun f (Vfun x) = f x
_fun _ _        = TypeError

_pair :: (Vpair -> V') -> V -> V'
_pair f (Vpair x) = f x
_pair _ _         = TypeError

_alt :: (Valt -> V') -> V -> V'
_alt f (Valt x) = f x
_alt _ _         = TypeError

-- operator * z Reynoldsa (lift ogolny)
infixl 2 ^*
(^*) :: (V -> V') -> V' -> V'
f ^* (Vnorm z) = f z
_ ^* Verror = Verror
_ ^* TypeError = TypeError

-- doslownie jak w Reynolds, uzywajac sections przy liftowaniu, chyba slabo czytelne
binOpA :: V' -> (Vint -> Vint -> Vint) -> V' -> V'
binOpA a1 op a2 = (((\i1 -> (((\i2 -> Vnorm $ Vint (i1 `op` i2)) `_int`) ^*) a2) `_int`) ^*) a1

binOpB :: V' -> (Vbool -> Vbool -> Vbool) -> V' -> V'
binOpB b1 op b2 = (((\r1 -> (((\r2 -> Vnorm $ Vbool (r1 `op` r2)) `_bool`) ^*) b2) `_bool`) ^*) b1

binOpR :: V' -> (Vint -> Vint -> Vbool) -> V' -> V'
binOpR a1 op a2 = (((\i1 -> (((\i2 -> Vnorm $ Vbool (i1 `op` i2)) `_int`) ^*) a2) `_int`) ^*) a1

sem :: Exp -> Env -> V'
sem (N n) _ = Vnorm $ Vint n
sem (B b) _ = Vnorm $ Vbool b
sem (e1 :+: e2) env = binOpA (sem e1 env) (+) (sem e2 env)
sem (e1 :-: e2) env = binOpA (sem e1 env) (-) (sem e2 env)
sem (e1 :*: e2) env = binOpA (sem e1 env) (*) (sem e2 env)
sem (e1 :&&: e2) env = binOpB (sem e1 env) (&&) (sem e2 env)
sem (e1 :||: e2) env = binOpB (sem e1 env) (||) (sem e2 env)
sem (e1 :<=: e2) env = binOpR (sem e1 env) (<=) (sem e2 env)
sem (e1 :=: e2) env = binOpR (sem e1 env) (==) (sem e2 env)
sem (Not e) env = (((\b -> Vnorm $ Vbool $ not b) `_bool`) ^*) $ sem e env
sem (If cond t e) env = (((\b -> if b then sem t env else sem e env) `_bool`) ^*) $ sem cond env
sem (V ident) env = lookUp ident env
sem (Lambda ident e) env = Vnorm $ Vfun (\a -> sem e $ subst ident a env)
sem (App e e') env = (((\f -> ((\a -> f a) ^*) $ sem e' env) `_fun`) ^*) $ sem e env
sem (Let ident e' e) env = ((\v -> sem e $ subst ident v env) ^*) $ sem e' env
sem (Letrec ident lIdent e' e) env = 
    sem e env' where
        env' = subst ident (Vfun f0) env
        f0 = fix (\f a -> sem e' $ subst ident (Vfun f) $ subst lIdent a env)
sem (Pair e1 e2) env = ((\l -> ((\r -> Vnorm $ Vpair (l, r)) ^*) $ sem e2 env) ^*) $ sem e1 env
sem (Fst e) env = (((\(l, _) -> Vnorm l) `_pair`) ^*) $ sem e env
sem (Snd e) env = (((\(_, r) -> Vnorm r) `_pair`) ^*) $ sem e env
sem (Inl e) env = ((\v -> Vnorm $ Valt $ Vinl v) ^*) $ sem e env
sem (Inr e) env = ((\v -> Vnorm $ Valt $ Vinr v) ^*) $ sem e env
sem (Case alt idl el idr er) env = 
    (((\e -> case e of 
        Vinl v -> sem el $ subst idl v env
        Vinr v -> sem er $ subst idr v env
    )`_alt`) ^*) $ sem alt env
    