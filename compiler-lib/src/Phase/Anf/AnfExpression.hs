{-# LANGUAGE DeriveFunctor #-}

module Phase.Anf.AnfExpression where

import Core.Operator
import Core.Term
import Core.Types

-- TODO type this again!

data NExp s = AExp (AExp s)
            | CExp (CExp s)
            | NLet s (NExp s) (NExp s)
                deriving (Functor, Show)

data AExp s = ATerm (Type s) (Term s)
            | ALam  (Type s) [s] (NExp s)
            | AClo  (Type s) [s] [s] (NExp s)
            | AUnPrimOp UnOp (AExp s)
            | ABinPrimOp BinOp (AExp s) (AExp s)
                deriving (Functor, Show)

data CExp s = CIfThenElse (AExp s) (NExp s) (NExp s)
            | CApp (AExp s) [AExp s]
            | CCase (AExp s) [PExp s]
                deriving (Functor, Show)

data PExp s =
    PExp (NExp s) (NExp s) -- Maybe these can be less general?
        deriving (Functor, Show)