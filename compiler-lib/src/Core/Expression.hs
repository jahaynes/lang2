{-# LANGUAGE DeriveFunctor #-}

module Core.Expression ( Expr (..)
                       , Pattern (..)
                       , mapType
                       , typeOf
                       ) where

import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)

data Expr t s = Term       t (Term s)
              | Lam        t [s] (Expr t s)
              | App        t (Expr t s) [Expr t s]
              | Let        t s (Expr t s) (Expr t s)
              | UnPrimOp   t UnOp (Expr t s)
              | BinPrimOp  t BinOp (Expr t s) (Expr t s)
              | IfThenElse t (Expr t s) (Expr t s) (Expr t s)
              | Case       t (Expr t s) [Pattern t s]
                  deriving (Eq, Functor, Ord, Show)

data Pattern t s =
    Pattern (Expr t s) (Expr t s)
        deriving (Eq, Functor, Ord, Show)

mapType :: (t -> t)
        -> Expr t s
        -> Expr t s
mapType f expr =
    case expr of
        Term t term           -> Term (f t) term
        Lam t vs body         -> Lam (f t) vs (mapType f body)
        App t x xs            -> App (f t) (mapType f x) (map (mapType f) xs)
        Let t a b c           -> Let (f t) a (mapType f b) (mapType f c)
        UnPrimOp t o a        -> UnPrimOp (f t) o (mapType f a)
        BinPrimOp t o a b     -> BinPrimOp (f t) o (mapType f a) (mapType f b)
        IfThenElse t pr tr fl -> IfThenElse (f t) (mapType f pr) (mapType f tr) (mapType f fl)
        Case t scrut ps       -> Case (f t) (mapType f scrut) (map mapType' ps)

    where
    mapType' (Pattern a b) = Pattern (mapType f a) (mapType f b)

typeOf :: Expr t s -> t
typeOf expr =
    case expr of
      Term t _           -> t
      Lam t _ _          -> t
      App t _ _          -> t
      Let t _ _ _        -> t
      UnPrimOp t _ _     -> t
      BinPrimOp t _ _ _  -> t
      IfThenElse t _ _ _ -> t
      Case t _ _         -> t
