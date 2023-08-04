module Phase.LambdaLift.Alpha where

import           Data.Map        (Map)
import qualified Data.Map as M

import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule

alphas :: Ord s => AnfModule s -> AnfModule s
alphas md = md { getFunDefAnfTs = map alpha $ getFunDefAnfTs md }

alpha :: Ord s => FunDefAnfT s -> FunDefAnfT s
alpha (FunDefAnfT name q e) = FunDefAnfT name q (alphaNExp mempty e)

alphaNExp :: Ord s => Map s s -> NExp s -> NExp s
alphaNExp subst expr =

    case expr of

        AExp aexp -> AExp $ alphaAExp subst aexp

        CExp cexp -> CExp $ alphaCExp subst cexp

        NLet a (AExp (ATerm _ (Var b))) c ->
            alphaNExp (M.insert a b subst) c

        NLet a b c ->
            NLet a (alphaNExp subst b) (alphaNExp subst c)

alphaAExp :: Ord s => Map s s -> AExp s -> AExp s
alphaAExp subst aexp =

    case aexp of

        te@(ATerm t (Var v)) ->
            case M.lookup v subst of
                Just x  -> ATerm t (Var x)
                Nothing -> te

        te@ATerm{} ->
            te

        ALam t vs ex ->
            -- TODO. vs should probably mask out the subst here
            -- See FreeVars.hs
            ALam t vs (alphaNExp subst ex)

        AUnPrimOp t op ex ->
            AUnPrimOp t op (alphaAExp subst ex)

        ABinPrimOp t op a b ->
            ABinPrimOp t op (alphaAExp subst a) (alphaAExp subst b)

alphaCExp :: Ord s => Map s s -> CExp s -> CExp s
alphaCExp subst cexp =

    case cexp of

        -- Simple App-Lam beta reduction possible here?
        CApp t f xs ->
            CApp t (alphaAExp subst f) (map (alphaAExp subst) xs)

        CIfThenElse t pr tr fl ->
            CIfThenElse t (alphaAExp subst pr)
                          (alphaNExp subst tr)
                          (alphaNExp subst fl)

        CCase t scrut ps ->
            CCase t (alphaAExp subst scrut)
                    (map (alphaPExp subst) ps)

-- subst both sides?
alphaPExp :: Ord s => Map s s -> PExp s -> PExp s
alphaPExp subst (PExp a b) =
    -- TODO lhs should probably mask out the subst
    -- see FreeVars.hs
    PExp a (alphaNExp subst b)
