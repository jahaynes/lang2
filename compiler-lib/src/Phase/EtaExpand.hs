module Phase.EtaExpand where

import Common.State
import Core.Term
import TypeCheck.TypedExpression
import TypeCheck.Types

import Control.Monad         (replicateM)
import Data.ByteString.Char8 (ByteString, pack)

etaExpand :: TypedDefn Scheme ByteString -> TypedDefn Scheme ByteString
etaExpand (FunDefnT t s e) =
    FunDefnT t s (fst $ runState (etaExpandExpr genSym e) 0)

-- TODO: check the non-term cases as well
etaExpandExpr :: Show s => State Int s
                        -> TypedExpr Scheme s
                        -> State Int (TypedExpr Scheme s)

etaExpandExpr symGen = go
    where
    go (TermT t term) =
        if arity t == 0
            then pure $ TermT t term
            else do
              syms <- replicateM (arity t) symGen
              let (f:xs) = retype t (term:map Var syms)
              pure $ LamT t syms (AppT t f xs)

    go (LamT t vs body) =
        LamT t vs <$> go body

    go (AppT t f xs) =
        AppT t <$> go f <*> mapM go xs

    go (LetT t a b c) =
        LetT t a <$> go b <*> go c

    go (UnPrimOpT t o e) =
        UnPrimOpT t o <$> go e

    go (BinPrimOpT t o e1 e2) =
        BinPrimOpT t o <$> go e1 <*> go e2

    go (IfThenElseT ty p t f) =
        IfThenElseT ty <$> go p <*> go t <*> go f

genSym :: State Int ByteString
genSym = do
    n <- get
    let s = pack $ "e" <> show n
    put (n + 1)
    pure $ s

-- TODO check free vars
retype :: Scheme -> [Term s] -> [TypedExpr Scheme s]
retype scheme [x] = [TermT scheme x]
retype (Forall vs (TyArr t1 t2)) (f:xs) =
    TermT (Forall vs t1) f : retype (Forall vs t2) xs
retype _ _ = error "Bad retype input"