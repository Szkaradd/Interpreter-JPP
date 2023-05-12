module Interpreter.Expressions where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Szkarson.Abs
import Types
import Utils

evalExpr :: Expr -> IM Value
evalExpr (EVar _ ident) = readFromMem ident
evalExpr (EAddr _ ident) = do
  varEnv <- asks _varEnv
  return $ VInt $ varEnv Map.! ident
evalExpr (ELitInt _ int) = return $ VInt int
evalExpr (ELitTrue _) = return $ VBool True
evalExpr (ELitFalse _) = return $ VBool False
-- evalExpr (EApp _ ident exprs) = do
--   funcEnv <- asks _funcEnv
--   case Map.lookup ident funcEnv of
--     Nothing -> throwError $ "Function " ++ show ident ++ " not defined"
--     Just (args, block, env) -> do
--       vals <- mapM evalExpr exprs
--       env' <- local (const env) $ prepareArgs args vals
--       local (const env') $ evalBlock block

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
