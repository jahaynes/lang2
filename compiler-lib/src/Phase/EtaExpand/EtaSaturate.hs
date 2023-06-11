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
                      -> ExprT (Type s) s
                      -> ExprT (Type s) s
saturateExpr extraParams = go

    where
    go (TermT t term) =
        TermT t term

    go (LamT t vs body) =
        LamT t vs (go body)

    go (AppT t f@(TermT _ (Var fv)) xs) = do

        let f'  = go f
            xs' = map go xs

        case M.lookup fv extraParams of

            Nothing ->
                AppT t f' xs'

            Just eps -> do

                -- Append the extra (typed) arguments to the apply
                let xs'' = xs' ++ map (\(v, vt) -> TermT vt (Var v)) eps

                -- The extra variables for the fresh enclosing lambda
                let vs = map fst eps

                -- Calculate the type of the fresh enclosing lambda
                -- TODO untested
                let t' = foldr (TyArr . snd) t eps

                LamT t' vs (AppT t f' xs'')

    go (AppT t f xs) =
        AppT t (go f) (map go xs)

    go (LetT t a b c) =
        LetT t a (go b) (go c)

    go (UnPrimOpT t op e1) =
        UnPrimOpT t op (go e1)

    go (BinPrimOpT t op e1 e2) =
        BinPrimOpT t op (go e1) (go e2)

    go (IfThenElseT t pr tr fl) =
        IfThenElseT t (go pr) (go tr) (go fl)

    go (CaseT t scrut ps) =
        CaseT t (go scrut) (map goPat ps)

    goPat (PatternT a b) =
        PatternT (go a) (go b)