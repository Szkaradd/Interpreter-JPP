module TypeChecker.Program where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map)
import Data.Map qualified as Map
import Types
import Szkarson.Abs
import Utils
import TypeChecker.StatementsAndExpressions (checkBlock, checkStmt, checkStmts, processItems, toTType, initTypes, prepareArgs)

loadTopDef :: TopDef -> TM TypeEnv
loadTopDef (FnDef _ ident args retType block) = do
  env <- ask
  let funcTypeEnv = _typeFuncEnv env
  let funcTypeEnv' = Map.insert ident (args, toTType retType) funcTypeEnv
  let typeEnv = _typeEnv env
  let env' = env {_typeFuncEnv = funcTypeEnv'}
  env'' <- local (const env') $ prepareArgs args
  local (const env'') $ checkBlock block
  return env'
loadTopDef (GlobDecl _ varType items) = do
  types <- processItems varType items
  initTypes types

loadTopDefs :: [TopDef] -> TM TypeEnv
loadTopDefs [] = ask
loadTopDefs (x : xs) = do
  env <- loadTopDef x
  local (const env) $ loadTopDefs xs

checkProgram :: Prog -> TM ()
checkProgram (Program _ topDefs) = do
  env <- loadTopDefs topDefs
  case Map.lookup (Ident "main") (_typeFuncEnv env) of
    Nothing -> throwError "No main function"
    Just (_, retType) -> do
      if retType == IntT
        then return ()
        else throwError "Main function must return void"

typecheck :: Prog -> IO String
typecheck p = do
  let initialEnv = TypeEnv Map.empty Map.empty
  result <- runReaderT (runExceptT (checkProgram p)) initialEnv
  case result of
    (Left err) -> return err
    (Right _) -> return "OK"