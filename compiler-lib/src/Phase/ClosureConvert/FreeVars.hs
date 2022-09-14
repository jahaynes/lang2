module Phase.ClosureConvert.FreeVars (FreeVars (FreeVars, getFree), nexpFreeVars) where

import Common.State
import Core.Term
import Phase.Anf.AnfExpression

import           Data.Set      (Set, (\\))
import qualified Data.Set as S

data FreeVars s =
    FreeVars { getScope :: !(Set s)
             , getFree  :: !(Set s)
             }

getFreeVars :: Ord s => Set s -> NExp s -> [s]
getFreeVars topLevelScope e = S.toList
                            . getFree
                            . execState (nexpFreeVars e)
                            $ FreeVars { getScope = topLevelScope
                                       , getFree  = mempty }

nexpFreeVars :: Ord s => NExp s -> State (FreeVars s) ()
nexpFreeVars (AExp aexp)  = aexpFreeVars aexp
nexpFreeVars (CExp cexp)  = cexpFreeVars cexp
nexpFreeVars (NLet a b c) = do
    addToScope [a]
    nexpFreeVars b
    nexpFreeVars c
    removeFromScope [a]

aexpFreeVars :: Ord s => AExp s -> State (FreeVars s) ()
aexpFreeVars aexp =
    case aexp of
        ATerm t ->
            termFreeVars t
        ALam vs b -> do
            addToScope vs
            nexpFreeVars b
            removeFromScope vs
        ABinPrimOp _ a b ->
            mapM_ aexpFreeVars [a, b]

cexpFreeVars :: Ord s => CExp s -> State (FreeVars s) ()
cexpFreeVars cexp =
    case cexp of
        CApp f xs ->
            mapM_ aexpFreeVars (f:xs)
        CIfThenElse pr tr fl -> do
            aexpFreeVars pr
            nexpFreeVars tr
            nexpFreeVars fl

termFreeVars :: Ord s => Term s -> State (FreeVars s) ()
termFreeVars t =
    case t of
        Var s       -> tryInsert s
        LitInt{}    -> pure ()
        LitBool{}   -> pure ()
        LitString{} -> pure ()
        DCons{}     -> error "TODO DCons"

-- TODO: Is tryInsert redundant (set operation?)
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
