module TypeSystem.InferOperator where

import Common.State
import Core.Operator
import Core.Types
import TypeSystem.Common

import Data.ByteString (ByteString)
import Data.Functor    ((<&>))

unOp :: UnOp
     -> State (GroupState s) (Type ByteString)
unOp Negate = pure $ typeInt `TyArr` typeInt
unOp EShow  = freshTVar <&> \fr -> fr `TyArr` typeString
unOp Err    = freshTVar <&> \fr -> typeString `TyArr` fr

binOp :: BinOp
      -> State (GroupState s) (Type ByteString)
binOp AddI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp SubI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp MulI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp DivI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp ModI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)

binOp EqA = freshTVar <&> \fr -> fr `TyArr` (fr `TyArr` typeBool)

binOp LtEqI = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)
binOp LtI   = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)
binOp GtEqI = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)
binOp GtI   = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)

binOp AndB = pure $ typeBool `TyArr` (typeBool `TyArr` typeBool)
binOp OrB  = pure $ typeBool `TyArr` (typeBool `TyArr` typeBool)

binOp ConcatS  = pure $ typeString `TyArr` (typeString `TyArr` typeString)