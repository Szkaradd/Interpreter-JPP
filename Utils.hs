module Utils where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Szkarson.Abs
import Types

showIdent :: Ident -> String
showIdent (Ident str) = str

showPos :: Pos -> String
showPos (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c

invalidOperationError :: Show a => Value -> a -> Value -> Pos -> String
invalidOperationError val1 op val2 pos =
  "Invalid operation on values: "
    ++ show val1
    ++ " "
    ++ show op
    ++ " "
    ++ show val2
    ++ " at position: "
    ++ showPos pos

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
