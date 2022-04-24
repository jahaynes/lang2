module FreeVars.FreeVars where

import Common.State
import Core.Definition
import Core.Expression
import Core.Term

import           Data.Set      (Set, (\\))
import qualified Data.Set as S

data FreeVars s =
    FreeVars { getScope :: !(Set s)
             , getFree  :: !(Set s)
             }

getFreeVars :: Ord s => FunDefn s -> Set s
getFreeVars def = getFree
                . snd
                . runState (defnFreeVars def)
                $ FreeVars mempty mempty

defnFreeVars :: Ord s => FunDefn s -> State (FreeVars s) ()
defnFreeVars (FunDefn n e) = do
    addToScope [n]
    exprFreeVars e
    removeFromScope [n]

exprFreeVars :: Ord s => Expr s -> State (FreeVars s) ()
exprFreeVars e =

    case e of

        ETerm t ->
            termFreeVars t

        ELam vs b -> do
            addToScope vs
            exprFreeVars b
            removeFromScope vs

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
