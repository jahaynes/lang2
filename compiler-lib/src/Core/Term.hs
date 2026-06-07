{-# LANGUAGE DeriveFunctor #-}

module Core.Term where

data Term s = Var s
           -- | TlVar s -- yeah? nah?
            | DCons s
            | LitInt Integer
            | LitBool Bool     
            | LitString s
                deriving (Eq, Ord, Functor, Show)
