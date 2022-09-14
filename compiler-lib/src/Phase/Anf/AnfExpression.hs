module Phase.Anf.AnfExpression where

import Core.Operator
import Core.Term
--import Core.Types

-- TODO type this again!

data NExp s = AExp (AExp s)
            | CExp (CExp s)
            | NLet s (NExp s) (NExp s)
                deriving Show

data AExp s = ATerm (Term s)
            | ALam [s] (NExp s)
            | AClo [s] [s] (NExp s)
            | AUnPrimOp UnOp (AExp s)
            | ABinPrimOp BinOp (AExp s) (AExp s)
                deriving Show

data CExp s = CIfThenElse (AExp s) (NExp s) (NExp s)
            | CApp (AExp s) [AExp s]
                deriving Show
