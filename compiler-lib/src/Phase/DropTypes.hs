module Phase.DropTypes where

import Core.Expression
import Core.Module

dropTypes :: TFunDefn s -> FunDefn s
dropTypes (TFunDefn _ n e) = FunDefn n (dropTypes' e)

dropTypes' :: AExpr t s -> Expr s
dropTypes' aexpr =
    case aexpr of

        ATerm _ term ->
            ETerm term

        ALam _ vs body ->
            ELam vs (dropTypes' body)

        AApp _ f xs ->
            EApp (dropTypes' f) (map dropTypes' xs)

        ALet _ a b c ->
            ELet a (dropTypes' b) (dropTypes' c)

        AUnPrimOp _ op e ->
            EUnPrimOp op (dropTypes' e)

        ABinPrimOp _ op a b ->
            EBinPrimOp op (dropTypes' a) (dropTypes' b)

        AIfThenElse _ pr tr fl ->
            IfThenElse (dropTypes' pr) (dropTypes' tr) (dropTypes' fl)

        AClo{} ->
            error "closure"

        ACallClo{} ->
            error "call closure"
