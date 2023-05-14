module Interpreter.StatementsAndExpressions where

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

readFromStore :: Loc -> IM Value
readFromStore loc = do
  store <- gets _store
  return $ store Map.! loc

readFromEnv :: Ident -> IM Loc
readFromEnv ident = do
  env <- asks _varEnv
  return $ env Map.! ident

readVarValue :: Ident -> IM Value
readVarValue ident = do
  loc <- readFromEnv ident
  readFromStore loc

assignVar :: Ident -> Value -> IM Env
assignVar ident val = do
  env <- ask
  store <- get
  let loc = _varEnv env Map.! ident
  modify (\s -> s {_store = Map.insert loc val (_store s)})
  return env

prepareRefArg :: Ident -> Loc -> IM Env
prepareRefArg ident addr = do
  env <- ask
  return $ env {_varEnv = Map.insert ident addr (_varEnv env)}

prepareArgs :: [Arg] -> [Expr] -> Env -> IM Env
prepareArgs [] [] _ = ask
prepareArgs ((VArg _ t ident) : xs) (expr : ys) callEnv = do
  case expr of
    EAddr _ _ -> do
      VInt addr <- local (const callEnv) $ evalExpr expr
      env <- prepareRefArg ident addr
      local (const env) $ prepareArgs xs ys callEnv
    _ -> do
      val <- local (const callEnv) $ evalExpr expr
      env <- initVar ident val
      local (const env) $ prepareArgs xs ys callEnv

addFuncToEnv :: Ident -> [Arg] -> Block -> Type -> IM Env
addFuncToEnv ident args block retType = do
  env <- ask
  let funcEnv = _funcEnv env
  let funcEnv' = Map.insert ident (args, block, env {_funcEnv = funcEnv'}, retType) funcEnv
  return $ env {_funcEnv = funcEnv'}

---------------------- Built-in functions ----------------------

strCharAt :: Pos -> Ident -> [Expr] -> IM Value
strCharAt pos ident expr = do
  VString str <- readVarValue ident
  VInt index <- evalExpr $ head expr
  let index' = fromIntegral index
  when
    (index' < 0 || index' >= length str)
    (throwError $ "Index " ++ show index' ++ " out of bounds at: " ++ showPos pos)
  return $ VString [str !! index']

strLength :: Pos -> Ident -> IM Value
strLength pos ident = do
  VString str <- readVarValue ident
  return $ VInt (fromIntegral $ length str)

strSubstr :: Pos -> Ident -> [Expr] -> IM Value
strSubstr pos ident exprs = do
  VString str <- readVarValue ident
  VInt index1 <- evalExpr $ head exprs
  VInt index2 <- evalExpr $ head $ tail exprs
  let index1' = fromIntegral index1
  let index2' = fromIntegral index2
  when
    (index1' < 0 || index1' >= length str)
    (throwError $ "Index " ++ show index1' ++ " out of bounds at: " ++ showPos pos)
  when
    (index2' < 0 || index2' >= length str)
    (throwError $ "Index " ++ show index2' ++ " out of bounds at: " ++ showPos pos)
  return $ VString (take (index2' - index1') (drop index1' str))

strAppend :: Pos -> Ident -> [Expr] -> IM Value
strAppend pos ident exprs = do
  VString str <- readVarValue ident
  VString str' <- evalExpr $ head exprs
  return $ VString (str ++ str')

strReverse :: Pos -> Ident -> IM Value
strReverse pos ident = do
  VString str <- readVarValue ident
  return $ VString (reverse str)

strSetCharAt :: Pos -> Ident -> [Expr] -> IM Value
strSetCharAt pos ident exprs = do
  loc <- readFromEnv ident
  store <- get
  VString str <- readVarValue ident
  VInt index <- evalExpr $ head exprs
  VString char <- evalExpr $ head $ tail exprs
  let index' = fromIntegral index
  let char_len = length char
  when
    (index' < 0 || index' >= length str)
    (throwError $ "Index " ++ show index' ++ " out of bounds at: " ++ showPos pos)
  when
    (char_len /= 1)
    (throwError $ "String " ++ show char ++ " is not a character at: " ++ showPos pos)
  let str' = take index' str ++ char ++ drop (index' + 1) str
  modify $ \store -> store {_store = Map.insert loc (VString str') (_store store)}
  return $ VBool True

execBuildInFunc :: Pos -> Ident -> Ident -> [Expr] -> IM Value
execBuildInFunc pos varIdent funIdent exprs = do
  case funIdent of
    Ident "charAt" -> do
      strCharAt pos varIdent exprs
    Ident "length" -> do
      strLength pos varIdent
    Ident "substr" -> do
      strSubstr pos varIdent exprs
    Ident "append" -> do
      strAppend pos varIdent exprs
    Ident "reverse" -> do
      strReverse pos varIdent
    Ident "setCharAt" -> do
      strSetCharAt pos varIdent exprs
    _ -> throwError $ "Function " ++ show funIdent ++ " not defined"

---------------------- Expressions ----------------------

evalExpr :: Expr -> IM Value
evalExpr (EVar _ ident) = readVarValue ident
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
    Just (args, block, env, retType) -> do
      callEnv <- ask
      env' <- local (const env) $ prepareArgs args exprs callEnv
      env' <- local (const env') $ addFuncToEnv ident args block retType
      (env'', retVal) <- local (const env') $ evalBlock block
      case retVal of
        VReturn val -> return val
        VBlank -> return $ defaultValue retType
evalExpr (EBuildInFun pos varIdent funIdent exprs) = do
  execBuildInFunc pos varIdent funIdent exprs
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
      if val2 == 0 then
        throwError $ "Division by 0 at: " ++ showPos pos
      else return $ VInt $ val1 `div` val2
    Mod _ -> do
      if val2 == 0 then
        throwError $ "Modulo by 0 at: " ++ showPos pos
      else return $ VInt $ val1 `mod` val2
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
evalExpr (EAnd _ expr1 expr2) = do
  VBool val1 <- evalExpr expr1
  VBool val2 <- evalExpr expr2
  return $ VBool (val1 && val2)
evalExpr (EOr _ expr1 expr2) = do
  VBool val1 <- evalExpr expr1
  VBool val2 <- evalExpr expr2
  return $ VBool (val1 || val2)

evalExprs :: [Expr] -> IM [Value]
evalExprs [] = return []
evalExprs (x : xs) = do
  val <- evalExpr x
  vals <- evalExprs xs
  return (val : vals)

---------------------- Statements ----------------------

printVals :: [Value] -> IM ()
printVals [] = liftIO $ putStr "\n"
printVals (x : xs) = do
  liftIO $ putStr $ show x
  printVals xs

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

execElif :: ElseIf -> IM (Env, ReturnValue, Bool)
execElif (ElIf _ expr block) = do
  env <- ask
  val <- evalExpr expr
  case val of
    VBool True -> do
      (_, retVal) <- evalBlock block
      return (env, retVal, True)
    VBool False -> return (env, VBlank, False)

execElifs :: [ElseIf] -> IM (Env, ReturnValue, Bool)
execElifs [] = do
  env <- ask
  return (env, VBlank, False)
execElifs (x : xs) = do
  (env, val, bool) <- execElif x
  if bool then return (env, val, bool) else execElifs xs

evalDecl :: Decl -> IM (Env, ReturnValue)
evalDecl (DDecl _ t items) = do
  vals <- processItems t items
  env <- initVars vals
  return (env, VBlank)
evalDecl (FnDecl _ ident args retType block) = do
  env <- ask
  let funcEnv = _funcEnv env
  let funcEnv' = Map.insert ident (args, block, env, retType) funcEnv
  return (env {_funcEnv = funcEnv'}, VBlank)

evalBlock :: Block -> IM (Env, ReturnValue)
evalBlock (BBlock _ stmts) = execStmts stmts

execStmts :: [Stmt] -> IM (Env, ReturnValue)
execStmts [] = do
  env <- ask
  return (env, VBlank)
execStmts (x : xs) = do
  (env, val) <- execStmt x
  case val of
    VReturn v -> return (env, VReturn v)
    VBlank -> local (const env) $ execStmts xs

execStmt :: Stmt -> IM (Env, ReturnValue)
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
  val <- readVarValue ident
  val' <- case val of
    VInt v -> return $ VInt (v + 1)
  env <- assignVar ident val'
  return (env, VBlank)
execStmt (Decr _ ident) = do
  val <- readVarValue ident
  val' <- case val of
    VInt v -> return $ VInt (v - 1)
  env <- assignVar ident val'
  return (env, VBlank)
execStmt (Print _ exprs) = do
  env <- ask
  vals <- evalExprs exprs
  printVals vals
  return (env, VBlank)
execStmt (Assert pos expr) = do
  val <- evalExpr expr
  env <- ask
  case val of
    VBool True -> return (env, VBlank)
    VBool False -> throwError $ "Assertion failed at: " ++ showPos pos
execStmt (Ret _ expr) = do
  val <- evalExpr expr
  env <- ask
  return (env, VReturn val)
execStmt (VRet _) = do
  env <- ask
  return (env, VReturn VVoid)
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
      (env', val', bool) <- execElifs elifs
      if bool
        then return (env', val')
        else evalBlock elseBlock
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