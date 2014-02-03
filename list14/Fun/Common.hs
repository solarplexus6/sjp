module Fun.Common
( V (..)
, V' (..)
, Vint
, Vbool
, Vfun
, Vpair
, Env
, lookUp
, subst
) where

import qualified Data.Map as M (Map, insert, lookup)
import Text.Show.Functions()

import Fun.AbsSyntax

type Vint = Int
type Vbool = Bool
type Vfun = (V -> V')
type Vpair = (V, V)

data V = Vint Vint | Vbool Vbool | Vfun Vfun | Vpair Vpair deriving Show

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

data V' = Vnorm V | Error | TypeError deriving (Eq, Show)
type Env = M.Map Ident V

lookUp :: Ident -> Env -> V'
lookUp ident env = 
    case M.lookup ident env of
        Nothing -> Error
        Just v -> Vnorm v

subst :: Ident -> V -> Env -> Env
subst = M.insert