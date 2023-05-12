module Utils where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Szkarson.Abs

import Types

readFromStore :: Loc -> IM Value
readFromStore loc = do
  store <- gets _store
  return $ store Map.! loc

readFromEnv :: Ident -> IM Loc
readFromEnv ident = do
  env <- asks _varEnv
  return $ env Map.! ident

readFromMem :: Ident -> IM Value
readFromMem ident = do
  loc <- readFromEnv ident
  readFromStore loc

invalidOperationError :: Show a => Value -> a -> Value -> Pos -> String
invalidOperationError val1 op val2 pos =
  "Invalid operation on values: "
    ++ show val1
    ++ " "
    ++ show op
    ++ " "
    ++ show val2
    ++ " at position: "
    ++ show pos

initVar :: Ident -> Value -> IM Env
initVar ident val = do
  env <- ask
  store <- get
  let loc = _next store
  let env' = Env (Map.insert ident loc (_varEnv env)) (_funcEnv env)
  modify (\s -> s {_next = loc + 1, _store = Map.insert loc val (_store s)})
  return env'

initVars :: [(Ident, Value)] -> IM Env
initVars [] = ask
initVars ((ident, val) : xs) = do
  env <- initVar ident val
  local (const env) $ initVars xs

assignVar :: Ident -> Value -> IM Env
assignVar ident val = do
  env <- ask
  store <- get
  let loc = _varEnv env Map.! ident
  modify (\s -> s {_store = Map.insert loc val (_store s)})
  return env
