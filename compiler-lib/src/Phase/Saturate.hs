module Phase.Saturate (saturate) where

import Core.Expression
import Core.Module
import Core.Term
import Core.Types

import qualified Data.Map as M
import           Data.Maybe    (fromMaybe)

--saturate :: Ord s => TypedModule Scheme s -> TypedModule Scheme s
saturate md = md { getTFunDefns = map saturate' $ getTFunDefns md }

--saturate' :: Ord s => FunDefnT Scheme s -> FunDefnT Scheme s
saturate' (TFunDefn n e) = TFunDefn n (saturateExpr e)

--saturateExpr :: Ord s => TypedExpr Scheme s -> TypedExpr Scheme s
saturateExpr (ATerm t term) =
    ATerm t term

saturateExpr (ALam t vs body) =
    ALam t vs (saturateExpr body)

saturateExpr (AApp _ (ALam _ vs body) xs)

    -- TODO make sure not to duplicate work!
    | length vs == length xs = saturateExpr (substitute vs body xs)

saturateExpr (AApp t f xs) =
    AApp t (saturateExpr f) (map saturateExpr xs)

saturateExpr (ALet t a b c) =
    ALet t a (saturateExpr b) (saturateExpr c)

saturateExpr (AUnPrimOp t op e1) =
    AUnPrimOp t op (saturateExpr e1)

saturateExpr (ABinPrimOp t op e1 e2) =
    ABinPrimOp t op (saturateExpr e1) (saturateExpr e2)

saturateExpr (AIfThenElse t p tr f) =
    AIfThenElse t (saturateExpr p) (saturateExpr tr) (saturateExpr f)

-- substitute :: Ord s => [s] -> TypedExpr Scheme s -> [TypedExpr Scheme s] -> TypedExpr Scheme s
substitute vs' body' xs' = go (M.fromList $ zip vs' xs') body'

    where
    go subst term@(ATerm _ (Var v)) =
          fromMaybe term (M.lookup v subst)

    go _ t@ATerm{} = t

    -- TODO is this enough to prevent variable capture
    go subst (ALam t vs body) =
        let subst' = foldr M.delete subst vs
        in ALam t vs (go subst' body)

    go subst (AApp t f xs) =
        AApp t (go subst f) (map (go subst) xs)

    go subst (ALet t a b c) =
        let subst' = M.delete a subst
        in ALet t a (go subst' b) (go subst' c)

    go subst (AUnPrimOp t op e1) =
        AUnPrimOp t op (go subst e1)

    go subst (ABinPrimOp t op e1 e2) =
        ABinPrimOp t op (go subst e1) (go subst e2)

    go subst (AIfThenElse t p tr f) =
        AIfThenElse t (go subst p) (go subst tr) (go subst f)

