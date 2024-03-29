{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.ConstraintSolver where

import Core.Types
import TypeSystem.Common

import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.Map as M
import           Data.Set        (Set)
import qualified Data.Set as S

type Unifier s = (Subst s, [Constraint s])

data Constraint s =
    Constraint (Type s) (Type s)
        deriving (Eq, Show)

runSolve :: (Show s, Ord s) => [Constraint s] -> Either ByteString (Subst s)
runSolve cs = solver (Subst mempty, cs)

solver :: (Show s, Ord s) => Unifier s -> Either ByteString (Subst s)
solver (su, cs) =
    case cs of
        [] -> pure su
        (Constraint t1 t2: cs0) -> do
            su1  <- unifies t1 t2
            solver (su1 `compose` su, map (substituteConstraint su1) cs0)

compose :: (Show s, Ord s) => Subst s -> Subst s -> Subst s
(Subst s1) `compose` (Subst s2) = Subst $ M.map (substituteType (Subst s1)) s2 `M.union` s1

substituteConstraint :: (Ord s, Show s) => Subst s -> Constraint s -> Constraint s
substituteConstraint s (Constraint t1 t2) = Constraint (substituteType s t1) (substituteType s t2)

unifyMany :: (Show s, Ord s) => [Type s] -> [Type s] -> Either ByteString (Subst s)
unifyMany [] [] = pure $ Subst mempty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (map (substituteType su1) ts1)
                      (map (substituteType su1) ts2)
     return (su2 `compose` su1)
unifyMany _ _ = Left "unification mismatch"

unifies :: (Eq s, Ord s, Show s) => Type s -> Type s -> Either ByteString (Subst s)
unifies t1 t2 | t1 == t2 = pure (Subst mempty)
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]

unifies (TyCon tc1 as) (TyCon tc2 bs)
  | tc1 == tc2 = unifyMany as bs

-- TODO More here?

unifies t1 t2 = Left . pack $ "Unification fail: " ++ show t1 ++ " <-> " ++ show t2

bind :: (Eq s, Ord s) => s -> Type s -> Either ByteString (Subst s)
bind a t | t == TyVar a    = pure (Subst mempty)
         | occursCheck a t = Left "Infinite type"
         | otherwise       = pure (Subst $ M.singleton a t)

occursCheck :: Ord a => a -> Type a -> Bool
occursCheck a t = a `S.member` freeInType t

freeInType :: Ord s => Type s -> Set s
freeInType TyCon{}         = S.empty
freeInType (TyVar a)       = S.singleton a
freeInType (t1 `TyArr` t2) = freeInType t1 `S.union` freeInType t2
