{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Core.Types where

import           Data.ByteString       (ByteString)
import           Text.Printf           (printf)

data Untyped =
    Untyped deriving (Eq, Ord, Show)

data Type s = TyVar s
            | TyCon s [Type s]
            | TyArr (Type s) (Type s)
                deriving (Eq, Ord, Functor)

instance Show s => Show (Type s) where
    show (TyVar v) = show v
    show (TyCon c tvs) = printf "(%s)" (unwords (show c : map show tvs))
    show (TyArr t1 t2) = printf "(%s -> %s)" (show t1) (show t2)

data Polytype s =
    Forall [s] (Type s)
        deriving (Eq, Show)

typeBool, typeInt, typeString :: Type ByteString
typeBool   = TyCon "Bool" []
typeInt    = TyCon "Int" []
typeString = TyCon "String" []
