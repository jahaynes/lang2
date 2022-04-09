{-# LANGUAGE DeriveFunctor #-}

module Core.Definition where

import Core.Expression

data Defn s = FunDefn s (Expr s)
            | DataDefn s [s] [DataCon s]
            | TypeSig s (Type s)
                deriving (Eq, Functor, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Eq, Functor, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Eq, Functor, Show)

data Type s = TyVar s
            | TyCon s
            | TyArr (Type s) (Type s)
                deriving (Eq, Ord, Functor, Show)