{-# LANGUAGE DeriveFunctor #-}

module Core.Expression where

import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)
import Core.Types    (Type)

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

data ExprT s = TermT       (Type s) (Term s)
             | LamT        (Type s) [s] (ExprT s)
             | AppT        (Type s) (ExprT s) [ExprT s]
             | LetT        (Type s) s (ExprT s) (ExprT s)
             | UnPrimOpT   (Type s) UnOp (ExprT s)
             | BinPrimOpT  (Type s) BinOp (ExprT s) (ExprT s)
             | IfThenElseT (Type s) (ExprT s) (ExprT s) (ExprT s)
             | CaseT       (Type s) (ExprT s) [PatternT s]
                 deriving (Eq, Show)

data PatternT s =
    PatternT (ExprT s) (ExprT s)
        deriving (Eq, Show)

mapType :: (Type s -> Type s)
        -> ExprT s
        -> ExprT s
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

mapType' :: (Type s -> Type s)
         -> PatternT s
         -> PatternT s
mapType' f (PatternT a b) = PatternT (mapType f a) (mapType f b)         

typeOf :: ExprT s -> Type s
typeOf expr =
    case expr of
      TermT t _           -> t
      LamT t _ _          -> t
      AppT t _ _          -> t
      LetT t _ _ _        -> t
      UnPrimOpT t _ _     -> t
      BinPrimOpT t _ _ _  -> t
      IfThenElseT t _ _ _ -> t
