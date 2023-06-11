{-# LANGUAGE DeriveFunctor #-}

module Core.Expression where

import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)

data Expr s = ETerm (Term s)
            | ELam [s] (Expr s)
            | EApp (Expr s) [Expr s]
            | ELet s (Expr s) (Expr s)
            | EUnPrimOp UnOp (Expr s)
            | EBinPrimOp BinOp (Expr s) (Expr s)
            | IfThenElse (Expr s) (Expr s) (Expr s)
            | ECase (Expr s) [Pattern s]
                deriving (Eq, Functor, Ord, Show)

data Pattern s =
    Pattern (Expr s) (Expr s)
        deriving (Eq, Functor, Ord, Show)

data ExprT t s = TermT       t (Term s)
               | LamT        t [s] (ExprT t s)
               | AppT        t (ExprT t s) [ExprT t s]
               | LetT        t s (ExprT t s) (ExprT t s)
               | UnPrimOpT   t UnOp (ExprT t s)
               | BinPrimOpT  t BinOp (ExprT t s) (ExprT t s)
               | IfThenElseT t (ExprT t s) (ExprT t s) (ExprT t s)
               | CaseT       t (ExprT t s) [PatternT t s]
                   deriving (Eq, Show)

data PatternT t s =
    PatternT (ExprT t s) (ExprT t s)
        deriving (Eq, Show)

mapType :: (t -> t)
        -> ExprT t s
        -> ExprT t s
mapType f expr =
    case expr of
        TermT t term           -> TermT (f t) term
        LamT t vs body         -> LamT (f t) vs (mapType f body)
        AppT t x xs            -> AppT (f t) (mapType f x) (map (mapType f) xs)
        LetT t a b c           -> LetT (f t) a (mapType f b) (mapType f c)
        UnPrimOpT t o a        -> UnPrimOpT (f t) o (mapType f a)
        BinPrimOpT t o a b     -> BinPrimOpT (f t) o (mapType f a) (mapType f b)
        IfThenElseT t pr tr fl -> IfThenElseT (f t) (mapType f pr) (mapType f tr) (mapType f fl)
        CaseT t scrut ps       -> CaseT (f t) (mapType f scrut) (map (mapType' f) ps)

mapType' :: (t -> t)
         -> PatternT t s
         -> PatternT t s
mapType' f (PatternT a b) = PatternT (mapType f a) (mapType f b)         

typeOf :: ExprT t s -> t
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
