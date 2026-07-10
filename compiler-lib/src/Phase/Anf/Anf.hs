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
    FunDefAnfT s (Quant s) (Type s) [s] [s] (NExp s) -- name, quantifier, type, env, vars, body
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

newtype AClosEnv s =
    AClosEnv [s]
        deriving (Functor, Show)

                                         -- TODO Lits
data PPat s = PVar s                     -- just vars for now
            | PApp s (Type s) [PPat s]   -- fully applied dcons (recursive for nested patterns)
                deriving (Functor, Show)

data PExp s =
    PExp (PPat s) (NExp s)
        deriving (Functor, Show)
