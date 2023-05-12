module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Szkarson.Abs

type Loc = Integer

type Pos = BNFC'Position


data Value
  = VInt Integer
  | VBool Bool
  | VString String
  deriving (Eq)

instance Show Value where
  show (VInt int) = show int
  show (VBool bool) = show bool
  show (VString str) = str


defaultValue :: Type -> Value
defaultValue (TInt _) = VInt 0
defaultValue (TBool _) = VBool False
defaultValue (TStr _) = VString ""

type Func = ([Arg], Block, Env)

data Env = Env
  { _varEnv :: Map Ident Loc,
    _funcEnv :: Map Ident Func
  }

data Store = Store
  { _store :: Map Loc Value,
    _next :: Loc
  }

type IM a = ExceptT String (StateT Store (ReaderT Env IO)) a
