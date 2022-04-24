{-# LANGUAGE DeriveFunctor #-}

module Core.Definition where

import Core.Expression
import TypeCheck.Types
import TypeCheck.TypedExpression

data Module s =
    Module { getDataDefns :: [DataDefn s]
           , getTypeSigs  :: [TypeSig s]
           , getFunDefns  :: [FunDefn s]
           } deriving (Functor, Show)

data TypedModule t s =
    TypedModule { getDataDefnsT :: [DataDefn s]
                , getTypeSigsT  :: [TypeSig s]
                , getFunDefnsT  :: [FunDefnT t s]
                } deriving (Functor, Show)

data DataDefn s =
    DataDefn s [s] [DataCon s]
        deriving (Functor, Show)

data TypeSig s =
    TypeSig s (Type s)
        deriving (Functor, Show)

data FunDefn s =
    FunDefn s (Expr s)
        deriving (Functor, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Functor, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Functor, Show)
