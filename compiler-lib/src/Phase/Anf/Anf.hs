{-# LANGUAGE DeriveFunctor #-}

module Phase.Anf.Anf where

import Core.Module
import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)
import Core.Types    (Type)

data AnfModule s =
    AnfModule { getDataDefnAnfTs :: [DataDefn s]
              , getFunDefAnfTs   :: [FunDefAnfT s]
              } deriving Show

data FunDefAnfT s =
    FunDefAnfT s (Quant s) [s] (NExp s) -- name, quantifier, vars, body
        deriving Show

data NExp s = AExp (AExp s)
            | CExp (CExp s)
            | NLet (Type s) s (NExp s) (NExp s)
                deriving (Functor, Show)

data AExp s = ATerm      (Type s) (Term s)
            -- TODO - Data constructor?
                deriving (Functor, Show)

data CExp s = CUnPrimOp  (Type s) UnOp (AExp s)
            | CBinPrimOp (Type s) BinOp (AExp s) (AExp s)
            | CIfThenElse (Type s) (AExp s) (NExp s) (NExp s)
            | CApp        (Type s) (AExp s) [AExp s]
            | CAppClo     (Type s) (AExp s) (AClosEnv s) [AExp s]
            | CCase       (Type s) (AExp s) [PExp s]
                deriving (Functor, Show)

--         --    | ALam       (Type s) [s] (NExp s)
        --    | AClo       (Type s) [s] [s] (NExp s)

newtype AClosEnv s =
    AClosEnv [s]
        deriving (Functor, Show)

data PPat s = PVar s                     -- just vars for now
            | PApp s (Type s) [Term s]   -- fully applied dcons
                deriving (Functor, Show)

data PExp s =
    PExp (PPat s) (NExp s)
        deriving (Functor, Show)

-- Only for printing? Should not need these
typeOf :: NExp s -> Type s
typeOf (AExp aexp)  = typeOfAExp aexp
typeOf (CExp cexp)  = typeOfCExp cexp
typeOf (NLet t _ _ _) = t -- ?

typeOfAExp :: AExp s -> Type s
typeOfAExp (ATerm t _)          = t

typeOfCExp :: CExp s -> Type s
typeOfCExp (CUnPrimOp t _ _)    = t
typeOfCExp (CBinPrimOp t _ _ _) = t
typeOfCExp (CIfThenElse t _ _ _) = t
typeOfCExp (CApp t _ _)          = t
typeOfCExp (CAppClo t _ _ _)     = t
typeOfCExp (CCase t _ _)         = t
