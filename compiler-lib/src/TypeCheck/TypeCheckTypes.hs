{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeCheck.TypeCheckTypes where

import Core.Types

import Data.Map (Map, findWithDefault)

newtype TypeCheckPlan s =
    TypeCheckPlan [s]
        deriving (Foldable, Show)

newtype Subst s =
    Subst (Map s (Type s))
        deriving Show

substituteType :: Ord s => Subst s -> Type s -> Type s
substituteType         _       (TyCon a) = TyCon a
substituteType (Subst s)     t@(TyVar a) = findWithDefault t a s
substituteType         s (t1 `TyArr` t2) = substituteType s t1 `TyArr` substituteType s t2