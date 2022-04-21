module Phase.Saturate where

import Core.Term
import TypeCheck.TypedExpression
import TypeCheck.Types

import qualified Data.Map as M

saturate :: Ord s => TypedDefn Scheme s -> TypedDefn Scheme s
saturate (FunDefnT t n e) =
    FunDefnT t n (saturateExpr e)

saturateExpr :: Ord s => TypedExpr Scheme s -> TypedExpr Scheme s
saturateExpr (TermT t term) =
    TermT t term

saturateExpr (LamT t vs body) =
    LamT t vs (saturateExpr body)

saturateExpr (AppT _ (LamT _ vs body) xs)

    -- TODO make sure not to duplicate work!
    | length vs == length xs = saturateExpr (substitute vs body xs)

saturateExpr (AppT t f xs) =
    AppT t (saturateExpr f) (map saturateExpr xs)

saturateExpr (LetT t a b c) =
    LetT t a (saturateExpr b) (saturateExpr c)

saturateExpr (UnPrimOpT t op e1) =
    UnPrimOpT t op (saturateExpr e1)

saturateExpr (BinPrimOpT t op e1 e2) =
    BinPrimOpT t op (saturateExpr e1) (saturateExpr e2)

saturateExpr (IfThenElseT t p tr f) =
    IfThenElseT t (saturateExpr p) (saturateExpr tr) (saturateExpr f)

substitute :: Ord s => [s] -> TypedExpr Scheme s -> [TypedExpr Scheme s] -> TypedExpr Scheme s
substitute vs' body' xs' = go (M.fromList $ zip vs' xs') body'

    where
    go subst term@(TermT _ (Var v)) =
        case M.lookup v subst of
            Just u  -> u
            Nothing -> term

    go _ t@TermT{} = t

    -- TODO is this enough to prevent variable capture
    go subst (LamT t vs body) =
        let subst' = foldr M.delete subst vs
        in LamT t vs (go subst' body)

    go subst (AppT t f xs) =
        AppT t (go subst f) (map (go subst) xs)

    go subst (LetT t a b c) =
        let subst' = M.delete a subst
        in LetT t a (go subst' b) (go subst' c)

    go subst (UnPrimOpT t op e1) =
        UnPrimOpT t op (go subst e1)

    go subst (BinPrimOpT t op e1 e2) =
        BinPrimOpT t op (go subst e1) (go subst e2)

    go subst (IfThenElseT t p tr f) =
        IfThenElseT t (go subst p) (go subst tr) (go subst f)

