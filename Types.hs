module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Szkarson.Abs

---------------------- Common types ----------------------

type Loc = Integer

type Pos = BNFC'Position

---------------------- Interpreter types ----------------------

data StmtReturnValue = VReturn Value | VBlank
  deriving (Show, Eq)

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VVoid
  deriving (Eq)

instance Show Value where
  show (VInt int) = show int
  show (VBool bool) = toLower bool
    where
      toLower True = "true"
      toLower False = "false"
  show (VString str) = str
  show VVoid = "void"


defaultValue :: Type -> Value
defaultValue (TInt _) = VInt 0
defaultValue (TBool _) = VBool False
defaultValue (TStr _) = VString ""

type Func = ([Arg], Block, Env)

data Env = Env
  { _varEnv :: Map Ident Loc,
    _funcEnv :: Map Ident Func
  } deriving (Show)

data Store = Store
  { _store :: Map Loc Value,
    _next :: Loc
  }

type IM a = ExceptT String (StateT Store (ReaderT Env IO)) a

---------------------- Typechecker types ----------------------

data TType = IntT | BoolT | StrT | VoidT | RefT TType | FuncT [TType] TType
  deriving Eq

instance Show TType where
  show IntT = "int"
  show BoolT = "bool"
  show StrT = "string"
  show VoidT = "void"
  show (RefT t) = show t ++ "*"

data TypeEnv = TypeEnv
  { _typeEnv :: Map Ident TType,
    _typeFuncEnv :: Map Ident ([Arg], TType)
  }

type TM a = ExceptT String (ReaderT TypeEnv IO) a
