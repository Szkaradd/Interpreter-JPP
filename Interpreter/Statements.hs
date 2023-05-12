module Interpreter.Statements where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Interpreter.Expressions (evalExpr)
import Szkarson.Abs
import Types
import Utils


processItem :: Type -> Item -> IM (Ident, Value)
processItem t (NoInit _ ident) = return (ident, defaultValue t)
processItem t (Init _ ident expr) = do
  val <- evalExpr expr
  return (ident, val)

processItems :: Type -> [Item] -> IM [(Ident, Value)]
processItems _ [] = return []
processItems t (x : xs) = do
  val <- processItem t x
  vals <- processItems t xs
  return (val : vals)

evalDecl :: Decl -> IM Env
evalDecl (DDecl _ t items) = do
  vals <- processItems t items
  initVars vals
evalDecl (FnDecl _ ident args retType block) = do
  env <- ask
  let funcEnv = _funcEnv env
  let funcEnv' = Map.insert ident (args, block, env {_funcEnv = funcEnv'}) funcEnv
  return $ env {_funcEnv = funcEnv'}

evalBlock :: Block -> IM Env
evalBlock (BBlock _ stmts) = do
  env <- ask
  env' <- execStmts stmts
  local (const env') $ return env

-- evalStmt (SIf _ expr stmt elifs) = do
--   val <- evalExpr expr
--   case val of
--     VBool True -> evalStmt stmt
--     VBool False -> case elifs of
--       [] -> ask
--       (elif : xs) -> evalStmt elif

execStmts :: [Stmt] -> IM Env
execStmts [] = ask
execStmts (x : xs) = do
  env' <- execStmt x
  local (const env') $ execStmts xs

execStmt :: Stmt -> IM Env
execStmt (Empty _) = ask

execStmt (BStmt _ block) = evalBlock block

execStmt (DStmt _ decl) = evalDecl decl

execStmt (Ass _ ident expr) = do
  val <- evalExpr expr
  assignVar ident val

execStmt (Incr _ ident) = do
  val <- readFromMem ident
  val' <- case val of
    VInt v -> return $ VInt (v + 1)
  assignVar ident val'

execStmt (Decr _ ident) = do
  val <- readFromMem ident
  val' <- case val of
    VInt v -> return $ VInt (v - 1)
  assignVar ident val'

execStmt (Print _ expr) = do
  val <- evalExpr expr
  liftIO $ print val
  ask

execStmt (Ret _ expr) = do
  val <- evalExpr expr
  liftIO $ print val
  ask

  
