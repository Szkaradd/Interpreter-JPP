module TypeChecker.ErrorMessages where

import Szkarson.Abs
import Types
import Utils

---------------------- Error messages ----------------------

errorAtPos :: Pos -> String
errorAtPos pos = "Error at: " ++ showPos pos ++ "\n"

errorVariableNotInScope :: Pos -> Ident -> String
errorVariableNotInScope pos ident = errorAtPos pos ++ "Variable " ++ showIdent ident ++ " not in scope"

errorFunctionNotInScope :: Pos -> Ident -> String
errorFunctionNotInScope pos ident = errorAtPos pos ++ "Function " ++ showIdent ident ++ " not in scope"

errorVarHasWrongType :: Pos -> Ident -> TType -> TType -> String
errorVarHasWrongType pos ident expectedType actualType =
  errorAtPos pos ++ "Variable " ++ showIdent ident ++ " has wrong type. Expected: " ++ show expectedType ++ ", received: " ++ show actualType

errorWrongNumberOfArgs :: Pos -> Ident -> Int -> Int -> String
errorWrongNumberOfArgs pos ident expected actual =
  errorAtPos pos ++ "Function " ++ showIdent ident ++ " called with wrong number of arguments. Expected: " ++ show expected ++ ", received: " ++ show actual

errorExpectedTypes :: Pos -> Ident -> [TType] -> [TType] -> String
errorExpectedTypes pos ident expected actual =
  errorAtPos pos ++ "Wrong types of arguments to function " ++ showIdent ident ++  " Expected: " ++ show expected ++ ", received: " ++ show actual

errorNoBuiltInFunction :: Pos -> Ident -> String
errorNoBuiltInFunction pos ident = errorAtPos pos ++ "function " ++ showIdent ident ++ " is not a built-in function"

errorMainCalled :: Pos -> String
errorMainCalled pos = errorAtPos pos ++ "Function main cannot be called"

errorCannotNeg :: Pos -> TType -> String
errorCannotNeg pos t = errorAtPos pos ++ "Cannot use operator - on " ++ show t ++ " only integers can be negated using -"

errorCannotNot :: Pos -> TType -> String
errorCannotNot pos t = errorAtPos pos ++ "Cannot use operator ! on " ++ show t ++ " only booleans can be negated using !"

errorCannotAddOrSub :: Pos -> TType -> TType -> String
errorCannotAddOrSub pos t1 t2 = errorAtPos pos ++ "Cannot add/subtract " ++ show t1 ++ " with " ++ show t2

errorCannotMulDivMod :: Pos -> TType -> TType -> String
errorCannotMulDivMod pos t1 t2 = errorAtPos pos ++ "Cannot multiply/divide/modulo " ++ show t1 ++ " with " ++ show t2

errorCannotAndOr :: Pos -> TType -> TType -> String
errorCannotAndOr pos t1 t2 = errorAtPos pos ++ "Cannot use operators &&/|| on " ++ show t1 ++ " with " ++ show t2

errorCannotCompare :: Pos -> TType -> TType -> String
errorCannotCompare pos t1 t2 = errorAtPos pos ++ "Cannot compare " ++ show t1 ++ " with " ++ show t2

errorCannotCompareRelOp :: Pos -> RelOp -> TType -> TType -> String
errorCannotCompareRelOp pos relOp t1 t2 =
  errorAtPos pos ++ "Cannot compare " ++ show t1 ++ " with " ++ show t2 ++ " using " ++ showRelOp relOp

errorCannotAssing :: Pos -> TType -> TType -> String
errorCannotAssing pos t1 t2 = errorAtPos pos ++ "Cannot assign " ++ show t1 ++ " to " ++ show t2

errorCannotDeclareMain :: Pos -> String
errorCannotDeclareMain pos = errorAtPos pos ++ "Cannot declare main function inside another function"

errorCannotIncrDecr :: Pos -> TType -> String
errorCannotIncrDecr pos t = errorAtPos pos ++ "Cannot increment/decrement " ++ show t ++ " only integers can be incremented/decremented"

errorCannotAssert :: Pos -> TType -> String
errorCannotAssert pos t = errorAtPos pos ++ "Cannot assert value of type " ++ show t ++ " only booleans can be asserted"

errorOnlyBoolInIf :: Pos -> TType -> String
errorOnlyBoolInIf pos t = errorAtPos pos ++ "Only booleans can be used in if statement, got " ++ show t

errorOnlyBoolInWhile :: Pos -> TType -> String
errorOnlyBoolInWhile pos t = errorAtPos pos ++ "Only booleans can be used in while statement, got " ++ show t

errorInReturn :: Pos -> TType -> TType -> String
errorInReturn pos expected actual = errorAtPos pos ++ "Cannot return " ++ show actual ++ " from function with return type " ++ show expected