{-# LANGUAGE DeriveFunctor #-}

module Core.Module where

import Core.Expression
import Core.Types

-----------------------------------------------

data Module s =
    Module { getDataDefns :: [DataDefn s]
           , getTypeSigs  :: [TypeSig s]
           , getFunDefns  :: [FunDefn s]
           } deriving (Functor, Show)

data DataDefn s =
    DataDefn s [s] [DataCon s]
        deriving (Functor, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Functor, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Functor, Show)

data TypeSig s =
    TypeSig s (Type s)
        deriving (Functor, Show)

data FunDefn s =
    FunDefn s (Expr s)
        deriving (Eq, Functor, Ord, Show)

newtype Quant s =
    Quant [s]
        deriving (Eq, Show)

data FunDefnT s =
    FunDefnT s (Quant s) (ExprT s)
        deriving (Eq, Show)

data ModuleT s =
    ModuleT { getFunDefnTs :: [FunDefnT s]
            } deriving Show