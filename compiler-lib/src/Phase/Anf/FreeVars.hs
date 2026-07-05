module Phase.Anf.FreeVars where

import           Data.Set ((\\), Set, fromList, toList, union, unions)
import qualified Data.Set as S

import Core.Term     (Term (..))
import Phase.Anf.Anf

class FreeVars f where
    fvs :: Ord v => f v -> [v]

termFvs :: Term v -> [v]
termFvs (Var v) = [v]
termFvs _       = []

boundVarsOf :: PPat v -> [v]
boundVarsOf (PVar v)      = [v]
boundVarsOf (PApp _ _ ts) = [v | Var v <- ts]   -- TODO check this

instance FreeVars AExp where
    fvs (ATerm _ t) = termFvs t

instance FreeVars CExp where
    fvs (CUnPrimOp _ _ a)     = fvs a
    fvs (CBinPrimOp _ _ a b)  = toList
                              $ fromList (fvs a) `union` fromList (fvs b)
    fvs (CIfThenElse _ p a b) = toList
                              $ fromList (fvs p) `union` fromList (fvs a)
                                                 `union` fromList (fvs b)
    fvs (CApp _ f xs)         = toList
                              $ fromList (fvs f) `union`
                                  unions (map (fromList . fvs) xs)
    fvs (CAppClo _ f e xs)    = toList
                              $ fromList (fvs f) `union` fromList (fvs e)
                                                 `union`
                                  unions (map (fromList . fvs) xs)
    fvs (CCase _ s ps)        = toList
                              $ fromList (fvs s) `union`
                                  unions (map (fromList . fvs) ps)

instance FreeVars NExp where
    fvs (AExp a)        = fvs a
    fvs (CExp c)        = fvs c
    fvs (NLet _ v b c)  = toList
                        $ fromList (fvs b) `union` S.delete v (fromList (fvs c))

instance FreeVars PExp where
    fvs (PExp pat body) = toList
                        $ fromList (fvs body) \\ fromList (boundVarsOf pat)

instance FreeVars PPat where
    fvs _ = []

instance FreeVars AClosEnv where
    fvs (AClosEnv vs) = vs
