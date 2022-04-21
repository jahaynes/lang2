{-# LANGUAGE DeriveFunctor,
             FlexibleInstances #-}

module TypeCheck.Types where

import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Data.ByteString (ByteString)

data Type s = TyVar s
            | TyCon s
            | TyArr (Type s) (Type s)
                deriving (Eq, Ord, Functor, Show)

data Constraint =
    Constraint !(Type ByteString) !(Type ByteString)
        deriving Show

data Scheme = Forall [ByteString] (Type ByteString)
  deriving (Show, Eq, Ord)

newtype Subst = Subst (Map ByteString (Type ByteString))
  deriving (Eq, Ord, Show)

newtype TypeEnv =
    TypeEnv { types :: Map ByteString Scheme }
        deriving (Eq, Show)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv mempty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set ByteString

-- TODO unflexible this
instance Substitutable (Type ByteString) where
  apply _ (TyCon a)           = TyCon a
  apply (Subst s) t@(TyVar a) = M.findWithDefault t a s
  apply s (t1 `TyArr` t2)     = apply s t1 `TyArr` apply s t2

  ftv TyCon{}         = S.empty
  ftv (TyVar a)       = S.singleton a
  ftv (t1 `TyArr` t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr M.delete s as
  ftv (Forall as t) = ftv t `S.difference` S.fromList as

instance Substitutable Constraint where
   apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)
   ftv (Constraint t1 t2) = ftv t1 `S.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env
  ftv (TypeEnv env) = ftv $ M.elems env

arity :: Scheme -> Int
arity (Forall _ t) = go t
    where
    go TyCon{}      = 0
    go TyVar{}      = 0 -- error "unknown arity"
    go (TyArr _ t2) = 1 + go t2