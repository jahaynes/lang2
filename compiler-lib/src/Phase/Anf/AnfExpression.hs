{-# LANGUAGE DeriveFunctor #-}

module Phase.Anf.AnfExpression where

import Core.Operator
import Core.Term
import Core.Types

data NExp s = AExp (AExp s)
            | CExp (CExp s)
            | NLet s (NExp s) (NExp s)
                deriving (Functor, Show)

data AExp s = ATerm      (Type s) (Term s)
            | ALam       (Type s) [s] (NExp s)
            | AClo       (Type s) [s] [s] (NExp s)
            | AUnPrimOp  (Type s) UnOp (AExp s)
            | ABinPrimOp (Type s) BinOp (AExp s) (AExp s)
                deriving (Functor, Show)

data CExp s = CIfThenElse (Type s) (AExp s) (NExp s) (NExp s)
            | CApp        (Type s) (AExp s) [AExp s]
            | CCase       (Type s) (AExp s) [PExp s]
                deriving (Functor, Show)

data PExp s =
    PExp (NExp s) (NExp s) -- Maybe these can be less general?
        deriving (Functor, Show)

typeOf :: NExp s -> Type s
typeOf (AExp aexp)  = typeOfAExp aexp
typeOf (CExp cexp)  = typeOfCExp cexp
typeOf (NLet _ _ c) = typeOf c

typeOfAExp :: AExp s -> Type s
typeOfAExp (ATerm t _)          = t
typeOfAExp (ALam t _ _)         = t
typeOfAExp (AClo t _ _ _)       = t
typeOfAExp (AUnPrimOp t _ _)    = t
typeOfAExp (ABinPrimOp t _ _ _) = t

typeOfCExp :: CExp s -> Type s
typeOfCExp (CIfThenElse t _ _ _) = t
typeOfCExp (CApp t _ _)          = t
typeOfCExp (CCase t _ _)         = t