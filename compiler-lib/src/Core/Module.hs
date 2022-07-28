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
        deriving (Eq, Functor, Show)

-----------------------------------------------

data TypedModule s =
    TypedModule { getTFunDefns :: [TFunDefn s] -- TODO.  datatypes and typesigs
            } deriving Show

untypeModule :: TypedModule s -> Module s
untypeModule tm = Module { getDataDefns = []    -- TODO.  datatypes and typesigs
                         , getTypeSigs  = []
                         , getFunDefns  = map untypeDefn (getTFunDefns tm)
                         }

data TFunDefn s =
    TFunDefn s (ExprT s)
        deriving (Eq, Show)

untypeDefn :: TFunDefn s -> FunDefn s
untypeDefn (TFunDefn n aexpr) = error "TODO" -- FunDefn n (stripAnnot aexpr)

-----------------------------------------------