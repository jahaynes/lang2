module Cps.PreCps (preCps) where

import Core.Definition
import Core.Expression
import Core.Term

import           Data.Set (Set)
import qualified Data.Set as S

preCps :: Ord s => Defn s -> Defn s
preCps (FunDefn n e) = FunDefn n (go mempty Inspect e)
preCps e = e

data Inspect = Inspect | Skip

go :: Ord s => Set s -> Inspect -> Expr s -> Expr s

-- Replaces references to top-levels as function calls
go scope Inspect e@(ETerm (Var v))
    | S.member v scope = e
    | otherwise        = EApp e []

go _ _ e@ETerm{} =
    e

go scope _ (ELam vs e) =
    let scope' = scope <> S.fromList vs
    in ELam vs (go scope' Inspect e)

go scope _ (EApp f xs) =
    EApp (go scope Skip f) (map (go scope Inspect) xs)

go scope _ (ELet a b c) =
    let scope' = S.insert a scope
    in ELet a (go scope' Inspect b) (go scope' Inspect c)

go scope _ (EUnPrimOp op e1) =
    EUnPrimOp op (go scope Inspect e1)

go scope _ (EBinPrimOp op e1 e2) =
    EBinPrimOp op (go scope Inspect e1) (go scope Inspect e2)

go scope _ (IfThenElse p t f) =
    IfThenElse (go scope Inspect p) (go scope Inspect t) (go scope Inspect f)
