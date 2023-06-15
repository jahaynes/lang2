{-# LANGUAGE DeriveFunctor #-}

module Core.Expression ( Expr (..)
                       , PatternT (..)
                       , mapType
                       , typeOf
                       ) where

import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)

data Expr t s = TermT       t (Term s)
               | LamT        t [s] (Expr t s)
               | AppT        t (Expr t s) [Expr t s]
               | LetT        t s (Expr t s) (Expr t s)
               | UnPrimOpT   t UnOp (Expr t s)
               | BinPrimOpT  t BinOp (Expr t s) (Expr t s)
               | IfThenElseT t (Expr t s) (Expr t s) (Expr t s)
               | CaseT       t (Expr t s) [PatternT t s]
                   deriving (Eq, Functor, Ord, Show)

data PatternT t s =
    PatternT (Expr t s) (Expr t s)
        deriving (Eq, Functor, Ord, Show)

mapType :: (t -> t)
        -> Expr t s
        -> Expr t s
mapType f expr =
    case expr of
        TermT t term           -> TermT (f t) term
        LamT t vs body         -> LamT (f t) vs (mapType f body)
        AppT t x xs            -> AppT (f t) (mapType f x) (map (mapType f) xs)
        LetT t a b c           -> LetT (f t) a (mapType f b) (mapType f c)
        UnPrimOpT t o a        -> UnPrimOpT (f t) o (mapType f a)
        BinPrimOpT t o a b     -> BinPrimOpT (f t) o (mapType f a) (mapType f b)
        IfThenElseT t pr tr fl -> IfThenElseT (f t) (mapType f pr) (mapType f tr) (mapType f fl)
        CaseT t scrut ps       -> CaseT (f t) (mapType f scrut) (map mapType' ps)

    where
    mapType' (PatternT a b) = PatternT (mapType f a) (mapType f b)

typeOf :: Expr t s -> t
typeOf expr =
    case expr of
      TermT t _           -> t
      LamT t _ _          -> t
      AppT t _ _          -> t
      LetT t _ _ _        -> t
      UnPrimOpT t _ _     -> t
      BinPrimOpT t _ _ _  -> t
      IfThenElseT t _ _ _ -> t
      CaseT t _ _         -> t
