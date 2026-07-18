module Core.Expression.FreeVars ( Free (freeVars) ) where

import Data.List ((\\))

import Core.Expression (Expr(..), Pattern(..), PatLhsExpr(..))
import Core.Term (Term(..))

class Free f where
  freeVars :: Eq s => f s -> [s]

instance Free Term where
  freeVars (Var s)     = [s]
  freeVars DCons{}     = []
  freeVars LitInt{}    = []
  freeVars LitBool{}   = []
  freeVars LitString{} = []

instance Free (Expr t) where
  freeVars (Term _ term)           = freeVars term
  freeVars (Lam _ vs body)         = filter (`notElem` vs) (freeVars body)
  freeVars (App _ x xs)            = freeVars x ++ concatMap freeVars xs
  freeVars (Let _ a b c)           = filter (/= a) (freeVars b ++ freeVars c)
  freeVars (UnPrimOp _ _ a)        = freeVars a
  freeVars (BinPrimOp _ _ a b)     = freeVars a ++ freeVars b
  freeVars (IfThenElse _ pr tr fl) = freeVars pr ++ freeVars tr ++ freeVars fl
  freeVars (Case _ scrut ps)       = freeVars scrut ++ concatMap freeVars ps

instance Free (Pattern t) where
  freeVars (Pattern lhs rhs) = freeVars rhs \\ boundVars lhs
    where
      boundVars (PVar _ s)       = [s]
      boundVars (PDCons _ _ ps)  = concatMap boundVars ps

instance Free (PatLhsExpr t) where
  freeVars _ = []


