{-# LANGUAGE DeriveFunctor #-}

module Core.Module where

import Core.Expression
<<<<<<< HEAD:compiler-lib/src/Core/Module.hs
<<<<<<< HEAD:compiler-lib/src/Core/Module.hs
<<<<<<< HEAD:compiler-lib/src/Core/Module.hs
import Core.Types
=======
>>>>>>> remove old types implementation:compiler-lib/src/Core/Module.hs
=======
>>>>>>> remove old types implementation:compiler-lib/src/Core/Module.hs
=======
>>>>>>> remove old types implementation:compiler-lib/src/Core/Module.hs

data Module s =
    Module { getDataDefns :: [DataDefn s]
           , getFunDefns  :: [FunDefn s]
           } deriving (Functor, Show)

<<<<<<< HEAD
<<<<<<< HEAD:compiler-lib/src/Core/Module.hs
=======
>>>>>>> eta expansion
data TypedModule s =
    TypedModule { getTFunDefns :: [TFunDefn s]
                } deriving Show

<<<<<<< HEAD
=======
>>>>>>> remove old types implementation:compiler-lib/src/Core/Module.hs
=======
>>>>>>> eta expansion
data DataDefn s =
    DataDefn s [s] [DataCon s]
        deriving (Functor, Show)

data FunDefn s =
    FunDefn s (Expr s)
        deriving (Eq, Functor, Show)

data TFunDefn s =
    TFunDefn (Polytype s) s (AExpr (Type s) s)
        deriving (Eq, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Functor, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Functor, Show)
