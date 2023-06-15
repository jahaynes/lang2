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

data FunDefn t s =
    FunDefn s (Expr t s)
        deriving (Eq, Functor, Ord, Show)

newtype Quant s =
    Quant [s]
        deriving (Eq, Show)

data FunDefnT t s =
    FunDefnT s (Quant s) (Expr t s)
        deriving (Eq, Show)

data ModuleT t s =
    ModuleT { getDataDefnTs :: [DataDefn s]
            , getFunDefnTs  :: [FunDefnT t s]
            } deriving Show
