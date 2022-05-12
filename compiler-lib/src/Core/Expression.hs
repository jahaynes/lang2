{-# LANGUAGE DeriveFunctor #-}

module Core.Expression where

import Core.Operator (BinOp, UnOp)
import Core.Term     -- (Term)


type Expr s = RTerm (ExprF s)


-- With recursion schemes

               -- Visible in source code
data ExprF s e = FTerm (Term s)
               | FLam [s] e
               | FApp e [e]
               | FLet s e e
               | FUnPrimOp UnOp e
               | FBinPrimOp BinOp e e
               | FIfThenElse e e e

               -- Generated during compilation
               | FClo [s] [s] e
               | FCallClo s [s]
                   deriving (Eq, Functor, Show)

newtype RTerm f = In { out :: f (RTerm f) }


topDown, bottomUp :: Functor f => (RTerm f -> RTerm f) -> RTerm f -> RTerm f
topDown  f =     In . fmap (topDown f)  . out . f
bottomUp f = f . In . fmap (bottomUp f) . out



topDown1 :: Functor f => (a -> RTerm f -> RTerm f) -> RTerm f -> RTerm f
topDown1 f = In . fmap (topDown f)  . out . f



flattenTerm :: Expr s -> Expr s
flattenTerm (In (FLam _ e )) = e  -- remove all Parens
flattenTerm other = other       -- do nothing otherwise

flatten'' :: Expr s -> Expr s
flatten'' = bottomUp flattenTerm


scopy :: Expr s -> Expr s
scopy = topDown1 (\z e -> e)