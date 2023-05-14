module Interpreter.Program where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Interpreter.StatementsAndExpressions (evalBlock, execStmt, execStmts, processItems)
import Szkarson.Abs
import Types
import Utils

loadTopDef :: TopDef -> IM Env
loadTopDef (FnDef _ ident args retType block) = do
  env <- ask
  let funcEnv = _funcEnv env
  let funcEnv' = Map.insert ident (args, block, env, retType) funcEnv
  return $ env {_funcEnv = funcEnv'}
loadTopDef (GlobDecl _ t items) = do
  vals <- processItems t items
  initVars vals

loadTopDefs :: [TopDef] -> IM Env
loadTopDefs [] = ask
loadTopDefs (x : xs) = do
  env <- loadTopDef x
  local (const env) $ loadTopDefs xs

execProgram :: Prog -> IM ()
execProgram (Program _ topDefs) = do
  env <- loadTopDefs topDefs
  store <- get
  case Map.lookup (Ident "main") (_funcEnv env) of
    Nothing -> throwError "No main function"
    Just (_, block, _, retType) -> do
      (_, val) <- local (const env) $ evalBlock block
      case val of
        VReturn value -> do
          liftIO $ putStrLn ("Program returned " ++ show value)
          return ()
        VBlank -> return ()

interpret :: Prog -> IO (Either String ())
interpret p = do
  let initialEnv = Env Map.empty Map.empty
  let initialStore = Store Map.empty 0
  result <- runReaderT (runStateT (runExceptT (execProgram p)) initialStore) initialEnv
  case result of
    (Left err, _) -> do
      return $ Left err
    (Right _, _) -> return $ Right ()