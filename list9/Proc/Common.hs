module Proc.Common
( Sigma
, Domain (..)
, Omega (..)
, EnvP (..)
, MemAssoc
, Loc
, assign
, newVar
, newProc
, restore
, lookUp
, lookUpProc
, zipDomain) where

import qualified Data.Map as M (Map, insert, lookup, assocs, fromList, intersectionWith, elems, union)
import Control.Monad.State
import Text.Show.Functions()

import Proc.AbsSyntax

type MemState = M.Map Ident Numeral
type MemAssoc = [(Ident, Numeral)]

type Loc    = Int
type Memory = M.Map Loc Numeral
type Store  = (Memory, Loc)
type EnvV   = M.Map Ident Loc
data EnvP   = EnvP (M.Map Ident (EnvV, EnvP, Sigma Omega))
data Domain = D (EnvV, EnvP, Store)

type Sigma a = State Domain a

data Omega = IotaTerm Domain

new :: Loc -> Loc
new l = l + 1

assign :: Ident -> Numeral -> Domain -> Domain
assign v n (D (envV, envP, (sto, next))) = 
    case M.lookup v envV of
        Nothing -> error ("Undeclared variable `" ++ v ++ "'")
        Just loc -> D (envV, envP, (M.insert loc n sto, next))

newVar :: Ident -> Numeral -> Domain -> Domain
newVar v n (D (envV, envP, (sto, next))) = D (M.insert v next envV, envP, (M.insert next n sto, new next))

lookUp :: Ident -> Domain -> Numeral
lookUp v (D (envV, _, (sto, _))) = 
    case M.lookup v envV of
        Nothing  -> error ("Unbound variable `" ++ v ++ "'")
        Just loc -> case M.lookup loc sto of
            Nothing -> error ("Corrupted store")
            Just n  -> n

restore :: EnvV -> EnvV -> EnvV
restore = M.union

zipDomain :: Domain -> MemState
zipDomain (D (envV, _, (sto, _) )) = M.fromList $ M.elems $ M.intersectionWith (\v n -> (v, n)) (M.fromList $ map (\(k,v) -> (v,k)) $ M.assocs envV) sto

newProc :: Ident -> Sigma Omega -> Domain -> Domain
newProc p pc (D (envV, EnvP envP, sto)) = D (envV, EnvP $ M.insert p (envV, EnvP envP, pc) envP, sto)

lookUpProc :: Ident -> EnvP -> (EnvV, EnvP, Sigma Omega)
lookUpProc p (EnvP envP) = 
    case M.lookup p envP of
        Nothing  -> error ("Unbound procedure `" ++ p ++ "'")
        Just v -> v