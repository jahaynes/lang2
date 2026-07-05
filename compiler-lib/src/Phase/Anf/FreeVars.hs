module Phase.Anf.FreeVars where

import           Data.Set ((\\), Set, fromList, union, unions)
import qualified Data.Set as S

import Core.Term     (Term (..))
import Phase.Anf.Anf

class FreeVars f where
    fvs :: Ord v => f v -> Set v

termFvs :: Term v -> Set v
termFvs (Var v) = S.singleton v
termFvs _       = S.empty

boundVarsOf :: Ord v => PPat v -> Set v
boundVarsOf (PVar v)      = S.singleton v
boundVarsOf (PApp _ _ ts) = fromList [v | Var v <- ts]   -- TODO check this

instance FreeVars AExp where
    fvs (ATerm _ t) = termFvs t

instance FreeVars CExp where
    fvs (CUnPrimOp _ _ a)     = fvs a
    fvs (CBinPrimOp _ _ a b)  = fvs a `union` fvs b
    fvs (CIfThenElse _ p a b) = fvs p `union` fvs a `union` fvs b
    fvs (CApp _ f xs)         = fvs f `union` unions (map fvs xs)
    fvs (CAppClo _ f e xs)    = fvs f `union` fvs e `union` unions (map fvs xs)
    fvs (CCase _ s ps)        = fvs s `union` unions (map fvs ps)

instance FreeVars NExp where
    fvs (AExp a)        = fvs a
    fvs (CExp c)        = fvs c
    fvs (NLet _ v b c)  = fvs b `union` S.delete v (fvs c)

instance FreeVars PExp where
    fvs (PExp pat body) = fvs body \\ boundVarsOf pat

instance FreeVars PPat where
    fvs _ = S.empty

instance FreeVars AClosEnv where
    fvs (AClosEnv vs) = fromList vs
