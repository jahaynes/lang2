{-# LANGUAGE DeriveFunctor #-}

module Core.Definition where

import Core.Expression

data Module s =
    Module { getDataDefns :: [DataDefn s]
           , getFunDefns  :: [FunDefn s]
           } deriving (Functor, Show)

data DataDefn s =
    DataDefn s [s] [DataCon s]
        deriving (Functor, Show)

data FunDefn s =
    FunDefn s (Expr s)
        deriving (Eq, Functor, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Functor, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Functor, Show)
