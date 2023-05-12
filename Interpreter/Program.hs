module Interpreter.Program where

-- import Interpreter.Expressions (evalExpr)
import Interpreter.Statements (execStmt, execStmts, evalBlock, processItems)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Szkarson.Abs
import Types
import Utils

-- execFunc :: Func -> IM Env
-- execFunc (args, block, env) = do
--   env' <- prepareArgs args []
--   local (const env') $ evalBlock block

loadTopDef :: TopDef -> IM Env
loadTopDef (FnDef _ ident args _ block) = do
  env <- ask
  let funcEnv = _funcEnv env
  let funcEnv' = Map.insert ident (args, block, env {_funcEnv = funcEnv'}) funcEnv
  return $ env {_funcEnv = funcEnv'}

loadTopDef (GlobDecl _ t items) = do
  vals <- processItems t items
  initVars vals

loadTopDefs :: [TopDef] -> IM Env
loadTopDefs [] = ask
loadTopDefs (x : xs) = do
  env <- loadTopDef x
  local (const env) $ loadTopDefs xs

execProgr :: Prog -> IM Env
execProgr (Program _ topDefs) = do
  env <- loadTopDefs topDefs
  store <- get
  -- liftIO $ putStrLn "Loaded top definitions"
  -- liftIO $ putStrLn ("funcEnv = " ++ show (_funcEnv env))
  -- liftIO $ putStrLn ("varEnv = " ++ show (_varEnv env))
  -- liftIO $ putStrLn ("store = " ++ show (_store store))
  -- print all functions idents
  case Map.lookup (Ident "main") (_funcEnv env) of
    Nothing -> throwError "No main function"
    Just (_, block, _) -> do
      (_, val) <- local (const env) $ evalBlock block
      case val of
        VReturn value -> do
          liftIO $ putStrLn ("Program returned " ++ show value)
          return env
        VBlank -> return env

-- initializeFuncEnv :: Map Ident Func
-- initializeFuncEnv = Map.fromList
--   [ (Ident "strlen", ([VArg NoPos VInt (Ident "str")], BBlock NoPos [Ret NoPos (EApp NoPos (Ident "strlen") [EVar NoPos (Ident "str")])], Env Map.empty Map.empty))
--   ]

-- initializeEnv :: IM Env
-- initializeEnv = do
--   let funcEnv = initializeFuncEnv
--   let varEnv = Map.empty
--   return $ Env funcEnv varEnv

interpret :: Prog -> IO ()
interpret p = do
  let initialEnv = Env Map.empty Map.empty
  let initialStore = Store Map.empty 0
  result <- runReaderT (runStateT (runExceptT (execProgr p)) initialStore) initialEnv
  case result of
    (Left err, _) -> putStrLn err
    (Right _, _) -> return ()