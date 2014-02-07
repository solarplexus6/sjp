module Fun.Lazy.Domain
( V (..)
, V' (..)
, Vfun
, Vpair
, Valt (..)
, Env
, lookUp
, subst
) where

import qualified Data.Map as M (Map, insert, lookup)
import Text.Show.Functions()

import Fun.Lazy.AbsSyntax
import Fun.Common

type Vfun = (V' -> V')
type Vpair = (V', V')
data Valt = Vinl V' | Vinr V' deriving (Eq, Show)

data V = Vint Vint | Vbool Vbool | Vfun Vfun | Vpair Vpair | Valt Valt | Vunit Vunit deriving Show

instance Eq V where 
  Vfun _   == Vfun _   = True
  Vint i1  == Vint i2  = i1 == i2
  Vint _   == _        = False
  _        == Vint _   = False
  Vbool b1 == Vbool b2 = b1 == b2
  Vbool _  == _        = False
  _        == Vbool _  = False
  Vpair p1 == Vpair p2 = p1 == p2
  Vpair _  == _        = False
  _        == Vpair _  = False
  Valt  a1 == Valt  a2 = a1 == a2
  Valt  _  == _        = False
  _        == Valt  _  = False
  Vunit _  == Vunit _  = True
  Vunit _  == _        = False
  _        == Vunit _  = False

data V' = Vnorm V | Verror | TypeError deriving (Eq, Show)
type Env = M.Map Ident V'

lookUp :: Ident -> Env -> V'
lookUp ident env = 
    case M.lookup ident env of
        Nothing -> Verror
        Just v -> v

subst :: Ident -> V' -> Env -> Env
subst = M.insert