module Core.Term where

data Term s = Var s
            | DCons s
            | LitInt Integer
            | LitBool Bool     
            | LitString s
                deriving (Eq, Show)
