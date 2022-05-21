module Core.Types where

data Type s = TyVar s
            | TyCon s
            | TyArr (Type s) (Type s)
                deriving (Eq, Show)
