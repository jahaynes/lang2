{-# LANGUAGE DeriveFunctor #-}

module Core.Definition where

import Core.Expression

data Defn s = FunDefn s (Expr s)
            | TypeDefn s [TyVar s] [DataCon s]
                deriving (Eq, Functor, Show)

newtype TyVar s =
    TyVar s
        deriving (Eq, Functor, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Eq, Functor, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Eq, Functor, Show)