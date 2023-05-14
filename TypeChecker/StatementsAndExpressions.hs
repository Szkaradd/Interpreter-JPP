module TypeChecker.StatementsAndExpressions where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Szkarson.Abs
import Types
import Utils

---------------------- Auxiliary functions ----------------------

showRelOp :: RelOp -> String
showRelOp (LTH _) = "<"
showRelOp (LE _) = "<="
showRelOp (GTH _) = ">"
showRelOp (GE _) = ">="
showRelOp (EQU _) = "=="
showRelOp (NE _) = "!="

errorAtPos :: Pos -> String
errorAtPos pos = "Error at: " ++ showPos pos ++ "\n"

getType :: Pos -> Ident -> TM TType
getType pos ident = do
  typeEnv <- ask
  case Map.lookup ident (_typeEnv typeEnv) of
    Just t -> return t
    Nothing -> throwError $ errorAtPos pos ++ "Variable " ++ showIdent ident ++ " not in scope"

toTType :: Type -> TType
toTType (TInt _) = IntT
toTType (TBool _) = BoolT
toTType (TStr _) = StrT
toTType (TVoid _) = VoidT

checkIfArgTypeMatches :: TType -> TType -> Bool
checkIfArgTypeMatches (RefT t1) (RefT t2) = t1 == t2
checkIfArgTypeMatches (RefT t1) t2 = t1 == t2
checkIfArgTypeMatches t1 (RefT t2) = t1 == t2
checkIfArgTypeMatches t1 t2 = t1 == t2

checkIfArgTypesMatch :: [TType] -> [TType] -> Bool
checkIfArgTypesMatch [] [] = True
checkIfArgTypesMatch (x : xs) (y : ys) = checkIfArgTypeMatches x y && checkIfArgTypesMatch xs ys
checkIfArgTypesMatch _ _ = False

getArgType :: Arg -> TType
getArgType (VArg _ varType ident) = toTType varType

getArgTypes :: [Arg] -> [TType]
getArgTypes = map getArgType

getArgIdent :: Arg -> Ident
getArgIdent (VArg _ _ ident) = ident

---------------------- Check Built-in functions ----------------------

checkIfVarIsString :: Pos -> Ident -> TType -> TM TType
checkIfVarIsString pos varIdent retType = do
  varType <- getType pos varIdent
  case varType of
    StrT -> return retType
    _ -> throwError $ errorAtPos pos ++ "Variable " ++ showIdent varIdent ++ " is not of type string"

correctNumberOfArguments :: Pos -> Ident -> Int -> Int -> TM ()
correctNumberOfArguments pos funcIdent expected actual =
  when (expected /= actual) $ do
    throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " expects " ++ show expected ++ " arguments"

checkBuiltInFunction :: Pos -> Ident -> Ident -> [Expr] -> TM TType
checkBuiltInFunction pos varIdent funcIdent exprs = do
  case funcIdent of
    (Ident "charAt") -> do
      correctNumberOfArguments pos funcIdent 1 (length exprs)
      exprVar <- checkExpr (head exprs)
      case exprVar of
        IntT -> do
          checkIfVarIsString pos varIdent StrT
        _ -> throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " expects int as argument"
    (Ident "length") -> do
      correctNumberOfArguments pos funcIdent 0 (length exprs)
      checkIfVarIsString pos varIdent IntT
    (Ident "substr") -> do
      correctNumberOfArguments pos funcIdent 2 (length exprs)
      exprVar1 <- checkExpr (head exprs)
      exprVar2 <- checkExpr (exprs !! 1)
      case exprVar1 of
        IntT -> do
          case exprVar2 of
            IntT -> do
              checkIfVarIsString pos varIdent StrT
            _ -> throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " expects int as second argument"
        _ -> throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " expects int as first argument"
    (Ident "append") -> do
      correctNumberOfArguments pos funcIdent 1 (length exprs)
      exprVar <- checkExpr (head exprs)
      case exprVar of
        StrT -> do
          checkIfVarIsString pos varIdent StrT
        _ -> throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " expects string as argument"
    (Ident "reverse") -> do
      correctNumberOfArguments pos funcIdent 0 (length exprs)
      checkIfVarIsString pos varIdent StrT
    (Ident "setCharAt") -> do
      correctNumberOfArguments pos funcIdent 2 (length exprs)
      exprVar1 <- checkExpr (head exprs)
      exprVar2 <- checkExpr (exprs !! 1)
      case exprVar1 of
        IntT -> do
          case exprVar2 of
            StrT -> do
              checkIfVarIsString pos varIdent VoidT
            _ ->
              throwError $
                errorAtPos pos
                  ++ "Function "
                  ++ show funcIdent
                  ++ " expects string as second argument"
        _ -> throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " expects int as first argument"
    _ -> throwError $ errorAtPos pos ++ "Function " ++ show funcIdent ++ " is not a built-in function"

---------------------- Check expressions ----------------------

checkExpr :: Expr -> TM TType
checkExpr (EVar pos ident) = do
  env <- ask
  case Map.lookup ident (_typeEnv env) of
    Nothing -> throwError $ errorAtPos pos ++ "Variable " ++ showIdent ident ++ " not in scope"
    Just varType -> return varType
checkExpr (EAddr pos ident) = do
  env <- ask
  case Map.lookup ident (_typeEnv env) of
    Nothing -> throwError $ errorAtPos pos ++ "Variable " ++ showIdent ident ++ " not in scope"
    Just varType -> return $ RefT varType
checkExpr (ELitInt pos _) = return IntT
checkExpr (ELitTrue pos) = return BoolT
checkExpr (ELitFalse pos) = return BoolT
checkExpr (EApp pos ident exprs) = do
  when (ident == Ident "main") $ throwError $ errorAtPos pos ++ "Function main cannot be called"
  env <- ask
  case Map.lookup ident (_typeFuncEnv env) of
    Nothing -> throwError $ errorAtPos pos ++ "Function " ++ showIdent ident ++ " not in scope"
    Just (argTypes, retType) -> do
      exprTypes <- mapM checkExpr exprs
      if checkIfArgTypesMatch (getArgTypes argTypes) exprTypes
        then return retType
        else throwError $ errorAtPos pos ++ "Function " ++ showIdent ident ++ " called with wrong arguments"
checkExpr (EBuildInFun pos varIdent funIdent exprs) = checkBuiltInFunction pos varIdent funIdent exprs
checkExpr (EString pos _) = return StrT
checkExpr (Neg pos expr) = do
  exprType <- checkExpr expr
  if exprType == IntT
    then return IntT
    else throwError $ errorAtPos pos ++ "Only integers can be negated"
checkExpr (Not pos expr) = do
  exprType <- checkExpr expr
  if exprType == BoolT
    then return BoolT
    else throwError $ errorAtPos pos ++ "Only booleans can be negated"
checkExpr (EMul pos expr1 _ expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == IntT && expr2Type == IntT
    then return IntT
    else
      throwError $
        errorAtPos pos
          ++ "Cannot perform mul, div or mod operation on "
          ++ show expr1Type
          ++ " and "
          ++ show expr2Type
checkExpr (EAdd pos expr1 _ expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == IntT && expr2Type == IntT
    then return IntT
    else
      throwError $
        errorAtPos pos
          ++ "Cannot perform add or sub operation on "
          ++ show expr1Type
          ++ " and "
          ++ show expr2Type
checkExpr (ERel pos expr1 op expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  case op of
    (EQU pos') ->
      if expr1Type == expr2Type
        then return BoolT
        else
          throwError $
            errorAtPos pos'
              ++ "Cannot compare value of type "
              ++ show expr1Type
              ++ " with value of type "
              ++ show expr2Type
    (NE pos') ->
      if expr1Type == expr2Type
        then return BoolT
        else
          throwError $
            errorAtPos pos'
              ++ "Cannot compare value of type "
              ++ show expr1Type
              ++ " with value of type "
              ++ show expr2Type
    _ -> do
      if expr1Type == IntT && expr2Type == IntT
        then return BoolT
        else
          throwError $
            errorAtPos pos
              ++ "Cannot compare "
              ++ show expr1Type
              ++ " and "
              ++ show expr2Type
              ++ " by operator "
              ++ showRelOp op
checkExpr (EAnd pos expr1 expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == BoolT && expr2Type == BoolT
    then return BoolT
    else throwError $ errorAtPos pos ++ "Only booleans can be compared by &&"
checkExpr (EOr pos expr1 expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == BoolT && expr2Type == BoolT
    then return BoolT
    else throwError $ errorAtPos pos ++ "Only booleans can be compared by ||"

checkExprs :: [Expr] -> TM [TType]
checkExprs [] = return []
checkExprs (x : xs) = do
  xType <- checkExpr x
  xsType <- checkExprs xs
  return $ xType : xsType

---------------------- Check statements ----------------------

-- data StmtType = ReturnT TType | BlankT

initType :: Ident -> TType -> TM TypeEnv
initType ident val = do
  env <- ask
  return $ env {_typeEnv = Map.insert ident val (_typeEnv env)}

initTypes :: [(Ident, TType)] -> TM TypeEnv
initTypes [] = ask
initTypes ((ident, val) : xs) = do
  env <- initType ident val
  local (const env) $ initTypes xs

processItem :: Type -> Item -> TM (Ident, TType)
processItem varType (NoInit pos ident) = return (ident, toTType varType)
processItem varType (Init pos ident expr) = do
  exprType <- checkExpr expr
  let varType' = toTType varType
  if exprType == varType'
    then return (ident, varType')
    else throwError $ errorAtPos pos ++ "Cannot assign " ++ show exprType ++ " to " ++ show varType'

processItems :: Type -> [Item] -> TM [(Ident, TType)]
processItems varType [] = return []
processItems varType (x : xs) = do
  val <- processItem varType x
  vals <- processItems varType xs
  return (val : vals)

prepareArgs :: [Arg] -> TM TypeEnv
prepareArgs [] = ask
prepareArgs (arg : args) = do
  env <- ask
  let argType = getArgType arg
  let env' = env {_typeEnv = Map.insert (getArgIdent arg) argType (_typeEnv env)}
  local (const env') $ prepareArgs args

checkDecl :: Decl -> TM TypeEnv
checkDecl (DDecl pos varType items) = do
  env <- ask
  vals <- processItems varType items
  initTypes vals
checkDecl (FnDecl pos ident args retType block) = do
  when (ident == Ident "main") $ throwError $ errorAtPos pos ++ "Cannot declare function main inside another function"
  env <- ask
  let funcTypeEnv = _typeFuncEnv env
  let funcTypeEnv' = Map.insert ident (args, toTType retType) funcTypeEnv
  let typeEnv = _typeEnv env
  let env' = env {_typeFuncEnv = funcTypeEnv'}
  env'' <- local (const env') $ prepareArgs args
  let env''' = env'' {_in_func = toTType retType}
  local (const env'') $ checkBlock block
  return env'

checkStmts :: [Stmt] -> TM TypeEnv
checkStmts [] = ask
checkStmts (stmt : stmts) = do
  env <- checkStmt stmt
  local (const env) $ checkStmts stmts

checkBlock :: Block -> TM TypeEnv
checkBlock (BBlock _ stmts) = checkStmts stmts

checkAssign :: Pos -> Ident -> Expr -> TM TypeEnv
checkAssign pos ident expr = do
  exprType <- checkExpr expr
  env <- ask
  case Map.lookup ident (_typeEnv env) of
    Nothing -> throwError $ errorAtPos pos ++ "Variable " ++ showIdent ident ++ " not in scope"
    Just varType -> do
      if varType == exprType
        then return env
        else throwError $ errorAtPos pos ++ "Cannot assign " ++ show exprType ++ " to " ++ show varType

checkIncrDecr :: Pos -> Ident -> TM TypeEnv
checkIncrDecr pos ident = do
  env <- ask
  case Map.lookup ident (_typeEnv env) of
    Nothing -> throwError $ errorAtPos pos ++ "Variable " ++ showIdent ident ++ " not in scope"
    Just varType ->
      if varType == IntT
        then return env
        else throwError $ errorAtPos pos ++ "Only integers can be decremented or incremented"

checkPrint :: Pos -> [Expr] -> TM TypeEnv
checkPrint pos exprs = do
  env <- ask
  checkExprs exprs
  return env

checkAssert :: Pos -> Expr -> TM TypeEnv
checkAssert pos expr = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == BoolT
    then return env
    else throwError $ errorAtPos pos ++ "Only booleans can be asserted"

checkElif :: Pos -> ElseIf -> TM TypeEnv
checkElif pos (ElIf pos' expr block) = checkIf pos' expr block []

checkElifs :: Pos -> [ElseIf] -> TM TypeEnv
checkElifs pos [] = ask
checkElifs pos (x : xs) = do
  env <- ask
  env' <- checkElif pos x
  checkElifs pos xs

checkIf :: Pos -> Expr -> Block -> [ElseIf] -> TM TypeEnv
checkIf pos expr block elifs = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == BoolT
    then do
      checkBlock block
      checkElifs pos elifs
      return env
    else throwError $ errorAtPos pos ++ "Only booleans can be used as a condition in if statements"

checkIfElse :: Pos -> Expr -> Block -> Block -> [ElseIf] -> TM TypeEnv
checkIfElse pos expr block1 block2 elifs = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == BoolT
    then do
      checkBlock block1
      checkBlock block2
      checkElifs pos elifs
      return env
    else throwError $ errorAtPos pos ++ "Only booleans can be used as a condition in if statements"

checkWhile :: Pos -> Expr -> Block -> TM TypeEnv
checkWhile pos expr block = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == BoolT
    then do
      checkBlock block
      return env
    else throwError $ errorAtPos pos ++ "Only booleans can be used as a condition in while statements"

checkReturn :: Pos -> Expr -> TM TypeEnv
checkReturn pos expr = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == _in_func env
    then return env
    else throwError $ errorAtPos pos ++ "Cannot return " ++ show exprType ++ " from function with return type " ++ show (_in_func env)

checkVoidReturn :: Pos -> TM TypeEnv
checkVoidReturn pos = do
  env <- ask
  if _in_func env == VoidT
    then return env
    else throwError $ errorAtPos pos ++ "Cannot return void from function with return type " ++ show (_in_func env)

checkStmt :: Stmt -> TM TypeEnv
checkStmt (Empty _) = ask
checkStmt (BStmt _ block) = checkBlock block
checkStmt (DStmt _ decl) = checkDecl decl
checkStmt (Ass pos ident expr) = checkAssign pos ident expr
checkStmt (Incr pos ident) = checkIncrDecr pos ident
checkStmt (Decr pos ident) = checkIncrDecr pos ident
checkStmt (Print pos exprs) = checkPrint pos exprs
checkStmt (Assert pos expr) = checkAssert pos expr
checkStmt (SIf pos expr block elifs) = checkIf pos expr block elifs
checkStmt (SIfElse pos expr block1 elifs block2) = checkIfElse pos expr block1 block2 elifs
checkStmt (While pos expr block) = checkWhile pos expr block
checkStmt (Ret pos expr) = checkReturn pos expr
checkStmt (VRet pos) = checkVoidReturn pos
checkStmt (SExp pos expr) = do
  checkExpr expr
  ask
