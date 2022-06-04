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
