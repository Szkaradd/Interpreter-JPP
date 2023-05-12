module Interpreter.Statements where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
-- import Interpreter.Expressions (evalExpr)
import Szkarson.Abs
import Types
import Utils

---------------------- Auxiliary functions ----------------------

updateEnv :: Ident -> Loc -> IM Env
updateEnv ident addr = do
  env <- ask
  return $ env {_varEnv = Map.insert ident addr (_varEnv env)}

prepareArgs :: [Arg] -> [Expr] -> Env -> IM Env
prepareArgs [] [] _= ask
prepareArgs ((VArg _ t ident) : xs) (expr : ys) callEnv = do
  case expr of
    EAddr _ _ -> do
      VInt addr <- local (const callEnv) $ evalExpr expr
      env <- updateEnv ident addr
      local (const env) $ prepareArgs xs ys callEnv
    _ -> do
      val <- local (const callEnv) $ evalExpr expr
      env <- initVar ident val
      local (const env) $ prepareArgs xs ys callEnv
      
---------------------- Expressions ----------------------

evalExpr :: Expr -> IM Value
evalExpr (EVar _ ident) = readFromMem ident
evalExpr (EAddr _ ident) = do
  addr <- readFromEnv ident
  return $ VInt addr
evalExpr (ELitInt _ int) = return $ VInt int
evalExpr (ELitTrue _) = return $ VBool True
evalExpr (ELitFalse _) = return $ VBool False
evalExpr (EApp _ ident exprs) = do
  funcEnv <- asks _funcEnv
  case Map.lookup ident funcEnv of
    Nothing -> throwError $ "Function " ++ show ident ++ " not defined"
    Just (args, block, env) -> do
      callEnv <- ask
      env' <- local (const env) $ prepareArgs args exprs callEnv
      (env'', retVal) <- local (const env') $ evalBlock block
      case retVal of
        VReturn val -> return val
        VBlank -> return VVoid

evalExpr (EString _ str) = return $ VString str
evalExpr (Neg _ expr) = do
  VInt val <- evalExpr expr
  return $ VInt (-val)
evalExpr (Not _ expr) = do
  VBool val <- evalExpr expr
  return $ VBool (not val)
evalExpr (EMul pos expr1 mulop expr2) = do
  VInt val1 <- evalExpr expr1
  VInt val2 <- evalExpr expr2
  case mulop of
    Times _ -> return $ VInt (val1 * val2)
    Div _ -> do
      when
        (val2 == 0)
        (throwError $ "Division by 0 at position: " ++ show pos)
      return $ VInt $ val1 `div` val2
    Mod _ -> do
      when
        (val2 == 0)
        (throwError $ "Modulo by 0 at position: " ++ show pos)
      return $ VInt $ val1 `mod` val2
evalExpr (EAdd pos expr1 addop expr2) = do
  VInt val1 <- evalExpr expr1
  VInt val2 <- evalExpr expr2
  case addop of
    Plus _ -> return $ VInt (val1 + val2)
    Minus _ -> return $ VInt (val1 - val2)
evalExpr (ERel pos expr1 relop expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (VInt int1, VInt int2) -> case relop of
      LTH _ -> return $ VBool (int1 < int2)
      LE _ -> return $ VBool (int1 <= int2)
      GTH _ -> return $ VBool (int1 > int2)
      GE _ -> return $ VBool (int1 >= int2)
      EQU _ -> return $ VBool (int1 == int2)
      NE _ -> return $ VBool (int1 /= int2)
    (VBool bool1, VBool bool2) -> case relop of
      EQU _ -> return $ VBool (bool1 == bool2)
      NE _ -> return $ VBool (bool1 /= bool2)
    (VString str1, VString str2) -> case relop of
      EQU _ -> return $ VBool (str1 == str2)
      NE _ -> return $ VBool (str1 /= str2)
    _ ->
      throwError $
        invalidOperationError val1 relop val2 pos
evalExpr (EAnd _ expr1 expr2) = do
  VBool val1 <- evalExpr expr1
  VBool val2 <- evalExpr expr2
  return $ VBool (val1 && val2)
evalExpr (EOr _ expr1 expr2) = do
  VBool val1 <- evalExpr expr1
  VBool val2 <- evalExpr expr2
  return $ VBool (val1 || val2)


---------------------- Statements ----------------------

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

execElif :: ElseIf -> IM (Env, StmtReturnValue, Bool)
execElif (ElIf _ expr block) = do
  env <- ask
  val <- evalExpr expr
  case val of
    VBool True -> do
      (_, retVal) <- evalBlock block
      return (env, retVal, True)
    VBool False -> return (env, VBlank, False)

execElifs :: [ElseIf] -> IM (Env, StmtReturnValue, Bool)
execElifs [] = do
  env <- ask
  return (env, VBlank, False)
execElifs (x : xs) = do
  (env, val, bool) <- execElif x
  if bool then return (env, val, bool) else execElifs xs


evalDecl :: Decl -> IM (Env, StmtReturnValue)
evalDecl (DDecl _ t items) = do
  vals <- processItems t items
  env <- initVars vals
  return (env, VBlank)
evalDecl (FnDecl _ ident args retType block) = do
  env <- ask
  let funcEnv = _funcEnv env
  let funcEnv' = Map.insert ident (args, block, env {_funcEnv = funcEnv'}) funcEnv
  return (env {_funcEnv = funcEnv'}, VBlank)

evalBlock :: Block -> IM (Env, StmtReturnValue)
evalBlock (BBlock _ stmts) = do
  env <- ask
  (env', val) <- local (const env) $ execStmts stmts
  return (env', val)

execStmts :: [Stmt] -> IM (Env, StmtReturnValue)
execStmts [] = do
  env <- ask
  return (env, VBlank)
execStmts (x : xs) = do
  (env, val) <- execStmt x
  case val of
    VReturn v -> return (env, VReturn v)
    VBlank -> local (const env) $ execStmts xs

execStmt :: Stmt -> IM (Env, StmtReturnValue)
execStmt (Empty _) = do
  env <- ask
  return (env, VBlank)

execStmt (BStmt _ block) = evalBlock block

execStmt (DStmt _ decl) = evalDecl decl

execStmt (Ass _ ident expr) = do
  val <- evalExpr expr
  env <- assignVar ident val
  return (env, VBlank)

execStmt (Incr _ ident) = do
  val <- readFromMem ident
  val' <- case val of
    VInt v -> return $ VInt (v + 1)
  env <- assignVar ident val'
  return (env, VBlank)

execStmt (Decr _ ident) = do
  val <- readFromMem ident
  val' <- case val of
    VInt v -> return $ VInt (v - 1)
  env <- assignVar ident val'
  return (env, VBlank)

execStmt (Print _ expr) = do
  val <- evalExpr expr
  liftIO $ print val
  env <- ask
  return (env, VBlank)

execStmt (Ret _ expr) = do
  val <- evalExpr expr
  env <- ask
  return (env, VReturn val)

execStmt (SIf _ expr block elifs) = do
  val <- evalExpr expr
  case val of
    VBool True -> evalBlock block
    VBool False -> case elifs of
      [] -> do
        env <- ask
        return (env, VBlank)
      (x : xs) -> do
        (env, val', bool) <- execElifs elifs
        return (env, val')

execStmt (SIfElse _ expr block elifs elseBlock) = do
  env <- ask
  val <- evalExpr expr
  case val of
    VBool True -> evalBlock block
    VBool False -> do
      (env', val',bool) <- execElifs elifs
      if bool then return (env', val') else
        evalBlock elseBlock

execStmt (While pos expr block) = do
  env <- ask
  val <- evalExpr expr
  case val of
    VBool True -> do
      (env', val') <- evalBlock block
      case val' of
        VReturn v -> return (env', VReturn v)
        VBlank -> local (const env) $ execStmt (While pos expr block)
    VBool False -> return (env, VBlank)

execStmt (SExp _ expr) = do
  val <- evalExpr expr
  env <- ask
  return (env, VBlank)