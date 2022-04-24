module Phase.DiscardTypes where

import Core.Definition
import Core.Expression
import TypeCheck.TypedExpression

discardTypes :: FunDefnT t s -> FunDefn s
discardTypes (FunDefnT _ n te) =
    FunDefn n (discard te)

discard :: TypedExpr t s -> Expr s
discard (TermT _ term) =
    ETerm term

discard (LamT _ vs body) =
    ELam vs (discard body)

discard (AppT _ f xs) =
    EApp (discard f) (map discard xs)

discard (LetT _ a b c) =
    ELet a (discard b) (discard c)

discard (UnPrimOpT _ op e1) =
    EUnPrimOp op (discard e1)

discard (BinPrimOpT _ op e1 e2) =
    EBinPrimOp op (discard e1) (discard e2)

discard (IfThenElseT _ p t f) =
    IfThenElse (discard p) (discard t) (discard f)
