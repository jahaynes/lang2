module TypeSystem.Ftv where

import Core.Types

import           Data.Set      (Set)
import qualified Data.Set as S
import           Data.Map      (Map)
import qualified Data.Map as M

class Ftv a where
    ftv :: a -> Set s

ftvEnv :: Ord s => Map s (Polytype s) -> Set s
ftvEnv env = freeInList $ M.elems env
    where
    freeInList = foldr (S.union . freeInPolytype) S.empty

    freeInPolytype (Forall pas pt) = ftvType pt `S.difference` S.fromList pas

ftvType :: Ord s => Type s -> Set s
ftvType (TyCon _)   = mempty
ftvType (TyVar v)   = S.singleton v
ftvType (TyArr a b) = ftvType a <> ftvType b
