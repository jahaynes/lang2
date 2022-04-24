{-# LANGUAGE DeriveFunctor #-}

module TypeCheck.TypedExpression where

import Core.Operator   (BinOp, UnOp)
import Core.Term       (Term)

data FunDefnT t s =
    FunDefnT t s (TypedExpr t s)
        deriving (Functor, Show)

data TypedExpr t s = TermT       t (Term s)
                   | LamT        t [s] (TypedExpr t s)
                   | AppT        t (TypedExpr t s) [TypedExpr t s]
                   | LetT        t s (TypedExpr t s) (TypedExpr t s)
                   | UnPrimOpT   t UnOp (TypedExpr t s)
                   | BinPrimOpT  t BinOp (TypedExpr t s) (TypedExpr t s)
                   | IfThenElseT t (TypedExpr t s) (TypedExpr t s) (TypedExpr t s)
                       deriving (Functor, Show)

withTypes :: (a -> b)
          -> TypedExpr a s
          -> TypedExpr b s
withTypes tf (TermT       ty e)        = TermT (tf ty) e
withTypes tf (LamT        ty vs e)     = LamT (tf ty) vs (withTypes tf e)
withTypes tf (AppT        ty e es)     = AppT (tf ty) (withTypes tf e) (map (withTypes tf) es)
withTypes tf (LetT        ty s e1 e2)  = LetT (tf ty) s (withTypes tf e1) (withTypes tf e2)
withTypes tf (UnPrimOpT   ty op e)     = UnPrimOpT (tf ty) op (withTypes tf e)
withTypes tf (BinPrimOpT  ty op e1 e2) = BinPrimOpT (tf ty) op (withTypes tf e1) (withTypes tf e2)
withTypes tf (IfThenElseT ty p t f)    = IfThenElseT (tf ty) (withTypes tf p) (withTypes tf t) (withTypes tf f)
