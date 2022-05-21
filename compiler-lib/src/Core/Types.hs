<<<<<<< HEAD
{-# LANGUAGE DeriveFunctor #-}

module Core.Types where

import           Text.Printf           (printf)

data Type s = TyVar s
            | TyCon s
            | TyArr (Type s) (Type s)
                deriving (Eq, Functor)

instance Show s => Show (Type s) where
    show (TyVar v) = show v
    show (TyCon c) = show c
    show (TyArr t1 t2) = printf "(%s -> %s)" (show t1) (show t2)

data Polytype s =
    Forall [s] (Type s)
        deriving (Eq, Show)

=======
module Core.Types where

data Type s = TyVar s
            | TyCon s
            | TyArr (Type s) (Type s)
                deriving (Eq, Show)
>>>>>>> remove old types implementation
