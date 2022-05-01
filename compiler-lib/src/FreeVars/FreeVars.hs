module FreeVars.FreeVars (FreeVars (FreeVars, getFree), exprFreeVars, getFreeVars) where

import Common.State
import Core.Expression
import Core.Term

import           Data.Set      (Set, (\\))
import qualified Data.Set as S

data FreeVars s =
    FreeVars { getScope :: !(Set s)
             , getFree  :: !(Set s)
             }

getFreeVars :: Ord s => Set s -> Expr s -> [s]
getFreeVars topLevelScope e = S.toList
                            . getFree
                            . snd
                            . runState (exprFreeVars e)
                            $ FreeVars { getScope = topLevelScope
                                       , getFree  = mempty }

exprFreeVars :: Ord s => Expr s -> State (FreeVars s) ()
exprFreeVars e =

    case e of

        ETerm t ->
            termFreeVars t

        ELam vs b -> do
            addToScope vs
            exprFreeVars b
            removeFromScope vs

        EClo fvs _ _ ->
            mapM_ tryInsert fvs

        EApp f xs -> do
            exprFreeVars f
            mapM_ exprFreeVars xs

        ELet a b c -> do
            addToScope [a]
            exprFreeVars b
            exprFreeVars c
            removeFromScope [a]

        EUnPrimOp _ a ->
            exprFreeVars a

        EBinPrimOp _ a b ->
            mapM_ exprFreeVars [a, b]

        IfThenElse p t f ->
            mapM_ exprFreeVars [p, t, f]

        CallClo{} ->
            error "TODO?"

termFreeVars :: Ord s => Term s -> State (FreeVars s) ()
termFreeVars t =
    case t of
        Var s       -> tryInsert s
        LitInt{}    -> pure ()
        LitBool{}   -> pure ()
        LitString{} -> pure ()
        _           -> error "TODO DCons"

tryInsert :: Ord s => s -> State (FreeVars s) ()
tryInsert v =
    modify' $ \fv ->
        if S.member v (getScope fv)
            then fv
            else fv { getFree = S.insert v (getFree fv) }

addToScope :: Ord s => [s] -> State (FreeVars s) ()
addToScope vs = modify' $ \fv -> fv { getScope = getScope fv <> S.fromList vs }

removeFromScope :: Ord s => [s] -> State (FreeVars s) ()
removeFromScope vs = modify' $ \fv -> fv { getScope = getScope fv \\ S.fromList vs }
