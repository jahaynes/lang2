module Phase.EtaExpand.EtaSaturate (etaSaturate) where

import Core.Expression
import Core.Module
import Core.Term
import Core.Types

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict as M

etaSaturate :: Ord s => ModuleT (Type s) s
                     -> Map s [(s, Type s)]
                     -> ModuleT (Type s) s
etaSaturate md extraParams =
    md { getFunDefnTs = map saturate' $ getFunDefnTs md }
    where
    saturate' (FunDefnT t n e) = FunDefnT t n (saturateExpr extraParams e)

saturateExpr :: Ord s => Map s [(s, Type s)]
                      -> Expr (Type s) s
                      -> Expr (Type s) s
saturateExpr extraParams = go

    where
    go (Term t term) =
        Term t term

    go (Lam t vs body) =
        Lam t vs (go body)

    go (App t f@(Term _ (Var fv)) xs) = do

        let f'  = go f
            xs' = map go xs

        case M.lookup fv extraParams of

            Nothing ->
                App t f' xs'

            Just eps -> do

                -- Append the extra (typed) arguments to the apply
                let xs'' = xs' ++ map (\(v, vt) -> Term vt (Var v)) eps

                -- The extra variables for the fresh enclosing lambda
                let vs = map fst eps

                -- Calculate the type of the fresh enclosing lambda
                -- TODO untested
                let t' = foldr (TyArr . snd) t eps

                Lam t' vs (App t f' xs'')

    go (App t f xs) =
        App t (go f) (map go xs)

    go (Let t a b c) =
        Let t a (go b) (go c)

    go (UnPrimOp t op e1) =
        UnPrimOp t op (go e1)

    go (BinPrimOp t op e1 e2) =
        BinPrimOp t op (go e1) (go e2)

    go (IfThenElse t pr tr fl) =
        IfThenElse t (go pr) (go tr) (go fl)

    go (Case t scrut ps) =
        Case t (go scrut) (map goPat ps)

    goPat (Pattern a b) =
        Pattern (go a) (go b)