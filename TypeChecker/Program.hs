module TypeChecker.Program where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import Data.Map qualified as Map
import Szkarson.Abs
import TypeChecker.StatementsAndExpressions (checkBlock, checkStmt, checkStmts, initTypes, prepareArgs, processItems, toTType)
import Types
import Utils

checkForMainRedefinition :: Pos ->Ident -> TM ()
checkForMainRedefinition pos ident = do
  env <- ask
  let funcEnv = _typeFuncEnv env
  when (ident == Ident "main") $ case Map.lookup ident funcEnv of
    Nothing -> return ()
    Just _ -> throwError $ "Redefinition of main function at " ++ showPos pos

loadTopDef :: TopDef -> TM TypeEnv
loadTopDef (FnDef pos ident args retType block) = do
  env <- ask
  checkForMainRedefinition pos ident
  let funcTypeEnv = _typeFuncEnv env
  let funcTypeEnv' = Map.insert ident (args, toTType retType) funcTypeEnv
  let typeEnv = _typeEnv env
  let env' = env {_typeFuncEnv = funcTypeEnv'}
  env'' <- local (const env') $ prepareArgs args
  let env''' = env'' {_in_func = toTType retType}
  local (const env''') $ checkBlock block
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
        else throwError "Main function must be of type int"

typecheck :: Prog -> IO String
typecheck p = do
  let initialEnv = TypeEnv Map.empty Map.empty IntT
  result <- runReaderT (runExceptT (checkProgram p)) initialEnv
  case result of
    (Left err) -> return err
    (Right _) -> return "OK"