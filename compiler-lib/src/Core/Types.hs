{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Core.Types where

import           Data.ByteString       (ByteString)
import           Text.Printf           (printf)

data Type s = TyVar s
            | TyCon s [s] -- Guessing how to implement type variables
            | TyArr (Type s) (Type s)
                deriving (Eq, Functor)

instance Show s => Show (Type s) where
    show (TyVar v) = show v
    show (TyCon c tvs) = printf "(%s)" (unwords $ map show (c:tvs))
    show (TyArr t1 t2) = printf "(%s -> %s)" (show t1) (show t2)

data Polytype s =
    Forall [s] (Type s)
        deriving (Eq, Show)

typeBool, typeInt, typeString :: Type ByteString
typeBool   = TyCon "Bool" []
typeInt    = TyCon "Int" []
typeString = TyCon "String" []
