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
            | Negate (Expr s)
                deriving (Eq, Show)
