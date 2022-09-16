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

        NLet a (AExp (ATerm (Var b))) c ->
            alphaNExp (M.insert a b subst) c

        NLet a b c ->
            NLet a (alphaNExp subst b) (alphaNExp subst c)

alphaAExp :: Ord s => Map s s -> AExp s -> AExp s
alphaAExp subst aexp =

    case aexp of

        t@(ATerm (Var v)) ->
            case M.lookup v subst of
                Just x  -> ATerm (Var x)
                Nothing -> t

        t@ATerm{} ->
            t

        ALam vs ex ->
            ALam vs (alphaNExp subst ex)

        AUnPrimOp op ex ->
            AUnPrimOp op (alphaAExp subst ex)

        ABinPrimOp op a b ->
            ABinPrimOp op (alphaAExp subst a) (alphaAExp subst b)

alphaCExp :: Ord s => Map s s -> CExp s -> CExp s
alphaCExp subst cexp =

    case cexp of

        -- Simple App-Lam beta reduction possible here?
        CApp f xs ->
            CApp (alphaAExp subst f) (map (alphaAExp subst) xs)

        CIfThenElse pr tr fl ->
            CIfThenElse (alphaAExp subst pr)
                        (alphaNExp subst tr)
                        (alphaNExp subst fl)
