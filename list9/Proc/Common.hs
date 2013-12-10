module Proc.Common
( Sigma
, Domain
, MemAssoc
, Loc
, assign
, restore
, lookUp
, zipDomain) where

import qualified Data.Map as M (Map, insert, lookup, assocs, fromList, intersectionWith, elems, union)
import Control.Monad.State

import Proc.AbsSyntax

type MemState = M.Map Ident Numeral
type MemAssoc = [(Ident, Numeral)]

type Loc    = Int
type Memory = M.Map Loc Numeral
type Store  = (Memory, Loc)
type EnvV   = M.Map Ident Loc
type Domain = (EnvV, Store)

type Sigma a = State Domain a

new :: Loc -> Loc
new l = l + 1

assign :: Ident -> Numeral -> Domain -> Domain
assign v n (envV, (sto, next)) = (M.insert v next envV, (M.insert next n sto, new next))

lookUp :: Ident -> Domain -> Numeral
lookUp v (envV, (sto, _)) = 
    case M.lookup v envV of
        Nothing  -> error ("Unbound variable `" ++ v ++ "'")
        Just loc -> case M.lookup loc sto of
            Nothing -> error ("Corrupted store")
            Just n  -> n

restore :: EnvV -> EnvV -> EnvV
restore = M.union

zipDomain :: Domain -> MemState
zipDomain (envV, (sto, _) ) = M.fromList $ M.elems $ M.intersectionWith (\v n -> (v, n)) (M.fromList $ map (\(k,v) -> (v,k)) $ M.assocs envV) sto