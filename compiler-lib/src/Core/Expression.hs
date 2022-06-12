{-# LANGUAGE DeriveFunctor #-}

module Core.Expression where

import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)

              -- Visible in source code
data Expr s = ETerm (Term s)
            | ELam [s] (Expr s)
            | EApp (Expr s) [Expr s]
            | ELet s (Expr s) (Expr s)
            | EUnPrimOp UnOp (Expr s)
            | EBinPrimOp BinOp (Expr s) (Expr s)
            | IfThenElse (Expr s) (Expr s) (Expr s)

              -- Generated during compilation
            | EClo [s] [s] (Expr s)
            | CallClo s [s]

                deriving (Eq, Functor, Show)

data AExpr a s = ATerm a (Term s)
               | ALam a [s] (AExpr a s)
               | AApp a (AExpr a s) [AExpr a s]
               | ALet a s (AExpr a s) (AExpr a s)
               | AUnPrimOp a UnOp (AExpr a s)
               | ABinPrimOp a BinOp (AExpr a s) (AExpr a s)
               | AIfThenElse a (AExpr a s) (AExpr a s) (AExpr a s)
               | AClo a [s] [s] (AExpr a s)
               | ACallClo a s [s]
                   deriving (Eq, Functor, Show)

annot :: AExpr a s -> a
annot expr =
    case expr of
      ATerm a _           -> a
      ALam a _ _          -> a
      AApp a _ _          -> a
      ALet a _ _ _        -> a
      AUnPrimOp a _ _     -> a
      ABinPrimOp a _ _ _  -> a
      AIfThenElse a _ _ _ -> a
      AClo{}              -> error "closure"
      ACallClo{}          -> error "call closure"

mapAnnot :: (a -> b) -> AExpr a s -> AExpr b s
mapAnnot f expr =
    case expr of
      ATerm a tm             -> ATerm (f a) tm
      ALam a vs body         -> ALam (f a) vs (mapAnnot f body)
      AApp a x ys            -> AApp (f a) (mapAnnot f x) (map (mapAnnot f) ys)
      ALet a x y z           -> ALet (f a) x (mapAnnot f y) (mapAnnot f z)
      AUnPrimOp a o e        -> AUnPrimOp (f a) o (mapAnnot f e)
      ABinPrimOp a o x y     -> ABinPrimOp (f a) o (mapAnnot f x) (mapAnnot f y)
      AIfThenElse a pr tr fa -> AIfThenElse (f a) (mapAnnot f pr) (mapAnnot f tr) (mapAnnot f fa)
      AClo{}                 -> error "closure"
      ACallClo{}             -> error "call closure"
