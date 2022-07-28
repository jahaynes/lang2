module Phase.DropTypes where

import Core.Expression
import Core.Module

dropModuleTypes :: TypedModule s -> Module s
dropModuleTypes tm = 
    Module { getDataDefns = [] -- TODO
           , getTypeSigs  = [] -- TODO
           , getFunDefns  = error "TODO" -- map dropFundefTypes $ getTFunDefns tm
           }

--dropFundefTypes :: TFunDefn s -> FunDefn s
--dropFundefTypes (TFunDefn n e) =
--    FunDefn n (dropExprTypes e)

dropExprTypes :: AExpr t s -> Expr s
dropExprTypes = go
    where
    go te =
      case te of
        (ATerm _ term) ->
            ETerm term

        (ALam _ vs body) ->
            ELam vs (go body)

        (AApp _ f xs) ->
            EApp (go f) (map go xs)
        
        (ALet _ a b c) ->
            ELet a (go b) (go c)

        (AUnPrimOp _ op e1) ->
            EUnPrimOp op (go e1)

        (ABinPrimOp _ op e1 e2) ->
            EBinPrimOp op (go e1) (go e2)

        (AIfThenElse _ p t f) ->
            IfThenElse (go p) (go t) (go f)

        AClo{} ->
            error "clo"

        ACallClo{} ->
            error "callclo"