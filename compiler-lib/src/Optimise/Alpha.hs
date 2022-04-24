-- Alpha-substitute unnecessary let-bindings
module Optimise.Alpha where

import           Data.Map        (Map)
import qualified Data.Map as M

import Core.Definition
import Core.Expression
import Core.Term

alphas :: Ord s => Module s -> Module s
alphas md = md { getFunDefns = map alpha $ getFunDefns md }

alpha :: Ord s => FunDefn s -> FunDefn s
alpha (FunDefn name e) = FunDefn name (alphaExpr mempty e)

alphaExpr :: Ord s => Map s (Term s) -> Expr s -> Expr s
alphaExpr subst e =

    case e of

        t@(ETerm (Var v)) ->
            case M.lookup v subst of
                Just x  -> ETerm x
                Nothing -> t

        t@ETerm{} ->
            t

        ELam vs ex ->
            ELam vs (alphaExpr subst ex)

        -- Simple App-Lam beta reduction possible here?
        EApp f xs ->
            EApp (alphaExpr subst f) (map (alphaExpr subst) xs)

        ELet a (ETerm b) c ->
            alphaExpr (M.insert a b subst) c

        ELet a b c ->
            ELet a (alphaExpr subst b) (alphaExpr subst c)

        EUnPrimOp op ex ->
            EUnPrimOp op (alphaExpr subst ex)

        EBinPrimOp op a b ->
            EBinPrimOp op (alphaExpr subst a) (alphaExpr subst b)

        IfThenElse p t f ->
            IfThenElse (alphaExpr subst p) (alphaExpr subst t) (alphaExpr subst f)
