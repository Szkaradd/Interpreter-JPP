module Utils where

import Szkarson.Abs
import Types

showIdent :: Ident -> String
showIdent (Ident str) = str

showPos :: Pos -> String
showPos (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c
showPos Nothing = ""

showRelOp :: RelOp -> String
showRelOp (LTH _) = "<"
showRelOp (LE _) = "<="
showRelOp (GTH _) = ">"
showRelOp (GE _) = ">="
showRelOp (EQU _) = "=="
showRelOp (NE _) = "!="

defaultValue :: Type -> Value
defaultValue (TInt _) = VInt 0
defaultValue (TBool _) = VBool False
defaultValue (TStr _) = VString ""