{-# LANGUAGE DeriveFunctor #-}

module Core.Expression where

import Core.Operator (BinOp, UnOp)
import Core.Term     (Term)
import Core.Types

import Debug.Trace (trace)

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



data ExprT s = TermT (Type s) (Term s)
             | LamT (Polytype s) [s] (ExprT s)
             | AppT (Type s) (ExprT s) [ExprT s]
             | LetT (Type s) s (ExprT s) (ExprT s)
             | UnPrimOpT (Type s) UnOp (ExprT s)
             | BinPrimOpT (Type s) BinOp (ExprT s) (ExprT s)
             | IfThenElseT (Type s) (ExprT s) (ExprT s) (ExprT s)
                 deriving (Eq, Show)
            -- TODO if necessary: clo/callclo

-- is it invalid to call this on a polytype?
annot' :: ExprT s -> Type s
annot' expr =
  case expr of
    TermT t _ -> t
    LamT (Forall _ t) _ _ -> trace "WARN-annot'-called-polytype" t
    AppT t _ _ -> t
    LetT t _ _ _ -> t
    UnPrimOpT t _ _ -> t
    BinPrimOpT t _ _ _ -> t
    IfThenElseT t _ _ _ -> t

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

-- probably the wrong approach
mapAnnot' :: (Type s -> Type s) -> ExprT s -> ExprT s
mapAnnot' f expr =
    case expr of
      TermT t tm             -> TermT (f t) tm
      LamT (Forall _ t) vs body -> LamT (undefined {-f t-}) vs (mapAnnot' f body)
      AppT t x ys            -> AppT (f t) (mapAnnot' f x) (map (mapAnnot' f) ys)
      LetT t x y z           -> LetT (f t) x (mapAnnot' f y) (mapAnnot' f z)
      UnPrimOpT t o e        -> UnPrimOpT (f t) o (mapAnnot' f e)
      BinPrimOpT t o x y     -> BinPrimOpT (f t) o (mapAnnot' f x) (mapAnnot' f y)
      IfThenElseT t pr tr fa -> IfThenElseT (f t) (mapAnnot' f pr) (mapAnnot' f tr) (mapAnnot' f fa)

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

stripAnnot :: AExpr t s -> Expr s
stripAnnot = go
  where
  go expr = 
    case expr of
      ATerm _ tm             -> ETerm tm
      ALam _ vs body         -> ELam vs (go body)
      AApp _ x ys            -> EApp (go x) (map go ys)
      ALet _ x y z           -> ELet x (go y) (go z)
      AUnPrimOp _ o e        -> EUnPrimOp o (go e)
      ABinPrimOp _ o x y     -> EBinPrimOp o (go x) (go y)
      AIfThenElse _ pr tr fa -> IfThenElse (go pr) (go tr) (go fa)
      AClo{}                 -> error "closure"
      ACallClo{}             -> error "call closure"
