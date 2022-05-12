{-# LANGUAGE DeriveFunctor #-}

module Core.Module where

import Core.Expression

data Module s =
    Module { getDataDefns :: [DataDefn s]
           , getFunDefns  :: [FunDefn s]
           }

data DataDefn s =
    DataDefn s [s] [DataCon s]

data FunDefn s =
    FunDefn s (Expr s)

data DataCon s =
    DataCon s [Member s]

data Member s = MemberType s
              | MemberVar s
