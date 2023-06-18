{-# LANGUAGE DeriveFunctor #-}

module Core.Module where

import Core.Expression
import Core.Types

-----------------------------------------------

data Module t s =
    Module { getDataDefns :: [DataDefn s]
           , getTypeSigs  :: [TypeSig s]
           , getFunDefns  :: [FunDefn t s]
           } deriving (Functor, Show)

data DataDefn s =
    DataDefn s [s] [DataCon s]
        deriving (Functor, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Functor, Show)

data Member s = MemberType s [Member s]
              | MemberVar s
                  deriving (Functor, Show)

data TypeSig s =
    TypeSig s (Type s)
        deriving (Functor, Show)

newtype Quant s =
    Quant [s]
        deriving (Eq, Ord, Show, Functor)

data FunDefn t s =
    FunDefn s (Quant s) (Expr t s)
        deriving (Eq, Ord, Show, Functor)
