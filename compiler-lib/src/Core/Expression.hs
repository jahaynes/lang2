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
            | InstantiateClos s [s]

              -- A runtime closure instance (fp + heap env ptr)
            | Instance s Int

                deriving (Eq, Functor, Show)
