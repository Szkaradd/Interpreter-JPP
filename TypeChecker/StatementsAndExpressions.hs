module TypeChecker.StatementsAndExpressions where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Szkarson.Abs
import Types
import Utils
import TypeChecker.ErrorMessages

---------------------- Auxiliary functions ----------------------

lookupVar :: Pos -> Ident -> TM TType
lookupVar pos ident = do
  env <- ask
  case Map.lookup ident (_typeVarEnv env) of
    Nothing -> throwError $ errorVariableNotInScope pos ident
    Just varType -> return varType

getType :: Pos -> Ident -> TM TType
getType pos ident = do
  typeEnv <- ask
  case Map.lookup ident (_typeVarEnv typeEnv) of
    Just t -> return t
    Nothing -> throwError $ errorVariableNotInScope pos ident

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
    _ -> throwError $ errorVarHasWrongType pos varIdent StrT varType

correctNumberOfArguments :: Pos -> Ident -> Int -> Int -> TM ()
correctNumberOfArguments pos funcIdent expected actual =
  when (expected /= actual) $ do
    throwError $ errorWrongNumberOfArgs pos funcIdent expected actual

checkBuiltInFunction :: Pos -> Ident -> Ident -> [Expr] -> TM TType
checkBuiltInFunction pos varIdent funcIdent exprs = do
  case funcIdent of
    (Ident "charAt") -> do
      correctNumberOfArguments pos funcIdent 1 (length exprs)
      exprType <- checkExpr (head exprs)
      case exprType of
        IntT -> do
          checkIfVarIsString pos varIdent StrT
        _ -> throwError $ errorExpectedTypes pos funcIdent [IntT] [exprType]
    (Ident "length") -> do
      correctNumberOfArguments pos funcIdent 0 (length exprs)
      checkIfVarIsString pos varIdent IntT
    (Ident "substr") -> do
      correctNumberOfArguments pos funcIdent 2 (length exprs)
      exprType1 <- checkExpr (head exprs)
      exprType2 <- checkExpr (exprs !! 1)
      case exprType1 of
        IntT -> do
          case exprType2 of
            IntT -> do
              checkIfVarIsString pos varIdent StrT
            _ -> throwError $ errorExpectedTypes pos funcIdent [IntT, IntT] [exprType1, exprType2]
        _ -> throwError $ errorExpectedTypes pos funcIdent [IntT, IntT] [exprType1, exprType2]
    (Ident "append") -> do
      correctNumberOfArguments pos funcIdent 1 (length exprs)
      exprType <- checkExpr (head exprs)
      case exprType of
        StrT -> do
          checkIfVarIsString pos varIdent StrT
        _ -> throwError $ errorExpectedTypes pos funcIdent [StrT] [exprType]
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
              throwError $ errorExpectedTypes pos funcIdent [IntT, StrT] [exprVar1, exprVar2]
        _ -> throwError $ errorExpectedTypes pos funcIdent [IntT, StrT] [exprVar1, exprVar2]
    _ -> throwError $ errorNoBuiltInFunction pos funcIdent

---------------------- Check expressions ----------------------

checkExpr :: Expr -> TM TType
checkExpr (EVar pos ident) = do
  env <- ask
  lookupVar pos ident
checkExpr (EAddr pos ident) = do
  env <- ask
  case Map.lookup ident (_typeVarEnv env) of
    Nothing -> throwError $ errorVariableNotInScope pos ident
    Just varType -> return $ RefT varType
checkExpr (ELitInt pos _) = return IntT
checkExpr (ELitTrue pos) = return BoolT
checkExpr (ELitFalse pos) = return BoolT
checkExpr (EApp pos ident exprs) = do
  when (ident == Ident "main") $ throwError $ errorMainCalled pos
  env <- ask
  case Map.lookup ident (_typeFuncEnv env) of
    Nothing -> throwError $ errorFunctionNotInScope pos ident
    Just (argTypes, retType) -> do
      exprTypes <- mapM checkExpr exprs
      if checkIfArgTypesMatch (getArgTypes argTypes) exprTypes
        then return retType
        else throwError $ errorExpectedTypes pos ident (getArgTypes argTypes) exprTypes
checkExpr (EBuildInFun pos varIdent funIdent exprs) = checkBuiltInFunction pos varIdent funIdent exprs
checkExpr (EString pos _) = return StrT
checkExpr (Neg pos expr) = do
  exprType <- checkExpr expr
  if exprType == IntT
    then return IntT
    else throwError $ errorCannotNeg pos exprType
checkExpr (Not pos expr) = do
  exprType <- checkExpr expr
  if exprType == BoolT
    then return BoolT
    else throwError $ errorCannotNot pos exprType
checkExpr (EMul pos expr1 _ expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == IntT && expr2Type == IntT
    then return IntT
    else
      throwError $ errorCannotMulDivMod pos expr1Type expr2Type
checkExpr (EAdd pos expr1 _ expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == IntT && expr2Type == IntT
    then return IntT
    else
      throwError $ errorCannotAddOrSub pos expr1Type expr2Type
checkExpr (ERel pos expr1 op expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  case op of
    (EQU pos') ->
      if expr1Type == expr2Type
        then return BoolT
        else
          throwError $ errorCannotCompare pos' expr1Type expr2Type
    (NE pos') ->
      if expr1Type == expr2Type
        then return BoolT
        else
          throwError $ errorCannotCompare pos' expr1Type expr2Type
    _ -> do
      if expr1Type == IntT && expr2Type == IntT
        then return BoolT
        else
          throwError $ errorCannotCompareRelOp pos op expr1Type expr2Type
checkExpr (EAnd pos expr1 expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == BoolT && expr2Type == BoolT
    then return BoolT
    else throwError $ errorCannotAndOr pos expr1Type expr2Type
checkExpr (EOr pos expr1 expr2) = do
  expr1Type <- checkExpr expr1
  expr2Type <- checkExpr expr2
  if expr1Type == BoolT && expr2Type == BoolT
    then return BoolT
    else throwError $ errorCannotAndOr pos expr1Type expr2Type

checkExprs :: [Expr] -> TM [TType]
checkExprs [] = return []
checkExprs (x : xs) = do
  xType <- checkExpr x
  xsType <- checkExprs xs
  return $ xType : xsType

---------------------- Check statements ----------------------

initType :: Ident -> TType -> TM TypeEnv
initType ident val = do
  env <- ask
  return $ env {_typeVarEnv = Map.insert ident val (_typeVarEnv env)}

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
    else throwError $ errorCannotAssing pos varType' exprType

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
  let env' = env {_typeVarEnv = Map.insert (getArgIdent arg) argType (_typeVarEnv env)}
  local (const env') $ prepareArgs args

checkDecl :: Decl -> TM TypeEnv
checkDecl (DDecl pos varType items) = do
  env <- ask
  vals <- processItems varType items
  initTypes vals
checkDecl (FnDecl pos ident args retType block) = do
  when (ident == Ident "main") $ throwError $ errorCannotDeclareMain pos
  env <- ask
  let funcTypeEnv = _typeFuncEnv env
  let funcTypeEnv' = Map.insert ident (args, toTType retType) funcTypeEnv
  let env' = env {_typeFuncEnv = funcTypeEnv'}
  env'' <- local (const env') $ prepareArgs args
  let env''' = env'' {_in_func = toTType retType}
  local (const env''') $ checkBlock block
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
  varType <- lookupVar pos ident
  if varType == exprType
    then return env
    else throwError $ errorCannotAssing pos exprType varType

checkIncrDecr :: Pos -> Ident -> TM TypeEnv
checkIncrDecr pos ident = do
  env <- ask
  varType <- lookupVar pos ident
  if varType == IntT
    then return env
    else throwError $ errorCannotIncrDecr pos varType

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
    else throwError $ errorCannotAssert pos exprType

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
    else throwError $ errorOnlyBoolInIf pos exprType

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
    else throwError $ errorOnlyBoolInIf pos exprType

checkWhile :: Pos -> Expr -> Block -> TM TypeEnv
checkWhile pos expr block = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == BoolT
    then do
      checkBlock block
      return env
    else throwError $ errorOnlyBoolInWhile pos exprType

checkReturn :: Pos -> Expr -> TM TypeEnv
checkReturn pos expr = do
  env <- ask
  exprType <- checkExpr expr
  if exprType == _in_func env
    then return env
    else throwError $ errorInReturn pos (_in_func env) exprType

checkVoidReturn :: Pos -> TM TypeEnv
checkVoidReturn pos = do
  env <- ask
  if _in_func env == VoidT
    then return env
    else throwError $ errorInReturn pos (_in_func env) VoidT

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
