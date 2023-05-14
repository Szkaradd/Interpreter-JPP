-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Szkarson.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Szkarson.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Szkarson.Abs.Ident -> Result
transIdent x = case x of
  Szkarson.Abs.Ident string -> failure x

transProg :: Show a => Szkarson.Abs.Prog' a -> Result
transProg x = case x of
  Szkarson.Abs.Program _ topdefs -> failure x

transTopDef :: Show a => Szkarson.Abs.TopDef' a -> Result
transTopDef x = case x of
  Szkarson.Abs.FnDef _ ident args type_ block -> failure x
  Szkarson.Abs.GlobDecl _ type_ items -> failure x

transArg :: Show a => Szkarson.Abs.Arg' a -> Result
transArg x = case x of
  Szkarson.Abs.VArg _ type_ ident -> failure x

transBlock :: Show a => Szkarson.Abs.Block' a -> Result
transBlock x = case x of
  Szkarson.Abs.BBlock _ stmts -> failure x

transDecl :: Show a => Szkarson.Abs.Decl' a -> Result
transDecl x = case x of
  Szkarson.Abs.DDecl _ type_ items -> failure x
  Szkarson.Abs.FnDecl _ ident args type_ block -> failure x

transStmt :: Show a => Szkarson.Abs.Stmt' a -> Result
transStmt x = case x of
  Szkarson.Abs.Empty _ -> failure x
  Szkarson.Abs.BStmt _ block -> failure x
  Szkarson.Abs.DStmt _ decl -> failure x
  Szkarson.Abs.Ass _ ident expr -> failure x
  Szkarson.Abs.Incr _ ident -> failure x
  Szkarson.Abs.Decr _ ident -> failure x
  Szkarson.Abs.Ret _ expr -> failure x
  Szkarson.Abs.VRet _ -> failure x
  Szkarson.Abs.SIf _ expr block elseifs -> failure x
  Szkarson.Abs.SIfElse _ expr block1 elseifs block2 -> failure x
  Szkarson.Abs.While _ expr block -> failure x
  Szkarson.Abs.Break _ -> failure x
  Szkarson.Abs.Continue _ -> failure x
  Szkarson.Abs.Print _ exprs -> failure x
  Szkarson.Abs.Assert _ expr -> failure x
  Szkarson.Abs.SExp _ expr -> failure x

transItem :: Show a => Szkarson.Abs.Item' a -> Result
transItem x = case x of
  Szkarson.Abs.NoInit _ ident -> failure x
  Szkarson.Abs.Init _ ident expr -> failure x

transElseIf :: Show a => Szkarson.Abs.ElseIf' a -> Result
transElseIf x = case x of
  Szkarson.Abs.ElIf _ expr block -> failure x

transType :: Show a => Szkarson.Abs.Type' a -> Result
transType x = case x of
  Szkarson.Abs.TInt _ -> failure x
  Szkarson.Abs.TStr _ -> failure x
  Szkarson.Abs.TBool _ -> failure x
  Szkarson.Abs.TVoid _ -> failure x

transExpr :: Show a => Szkarson.Abs.Expr' a -> Result
transExpr x = case x of
  Szkarson.Abs.EVar _ ident -> failure x
  Szkarson.Abs.EBuildInFun _ ident1 ident2 exprs -> failure x
  Szkarson.Abs.EAddr _ ident -> failure x
  Szkarson.Abs.ELitInt _ integer -> failure x
  Szkarson.Abs.ELitTrue _ -> failure x
  Szkarson.Abs.ELitFalse _ -> failure x
  Szkarson.Abs.EApp _ ident exprs -> failure x
  Szkarson.Abs.EString _ string -> failure x
  Szkarson.Abs.Neg _ expr -> failure x
  Szkarson.Abs.Not _ expr -> failure x
  Szkarson.Abs.EMul _ expr1 mulop expr2 -> failure x
  Szkarson.Abs.EAdd _ expr1 addop expr2 -> failure x
  Szkarson.Abs.ERel _ expr1 relop expr2 -> failure x
  Szkarson.Abs.EAnd _ expr1 expr2 -> failure x
  Szkarson.Abs.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => Szkarson.Abs.AddOp' a -> Result
transAddOp x = case x of
  Szkarson.Abs.Plus _ -> failure x
  Szkarson.Abs.Minus _ -> failure x

transMulOp :: Show a => Szkarson.Abs.MulOp' a -> Result
transMulOp x = case x of
  Szkarson.Abs.Times _ -> failure x
  Szkarson.Abs.Div _ -> failure x
  Szkarson.Abs.Mod _ -> failure x

transRelOp :: Show a => Szkarson.Abs.RelOp' a -> Result
transRelOp x = case x of
  Szkarson.Abs.LTH _ -> failure x
  Szkarson.Abs.LE _ -> failure x
  Szkarson.Abs.GTH _ -> failure x
  Szkarson.Abs.GE _ -> failure x
  Szkarson.Abs.EQU _ -> failure x
  Szkarson.Abs.NE _ -> failure x
