module Core.Definition where

import Core.Expression

data Defn s = FunDefn s (Expr s)
            | TypeDefn s [TyVar s] [DataCon s]
                deriving (Eq, Show)

newtype TyVar s =
    TyVar s
        deriving (Eq, Show)

data DataCon s =
    DataCon s [Member s]
        deriving (Eq, Show)

data Member s = MemberType s
              | MemberVar s
                  deriving (Eq, Show)