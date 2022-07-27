module Phase.EtaExpand (EtaState (..), etaExpand, expandDefn) where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types

import Data.ByteString.Char8 (ByteString, pack)

newtype EtaState =
    EtaState { _freshCount :: Int }

etaExpand :: TypedModule ByteString -> TypedModule ByteString
etaExpand md = md { getTFunDefns = evalState (mapM expandDefn $ getTFunDefns md) (EtaState 0) }

expandDefn :: TFunDefn ByteString
           -> State EtaState (TFunDefn ByteString)
expandDefn (TFunDefn n e) = TFunDefn n <$> expandExpr e

expandExpr :: Eq s => AExpr (Polytype s) ByteString
                   -> State EtaState (AExpr (Polytype s) ByteString)
expandExpr e@(ATerm (Forall _ t) term) =
    case (t, term) of
        (TyArr{}, Var f) -> functionCallToLambda f t
        (TyArr{},     _) -> error "Unexpected type arrow for non-var term"
        _                -> pure e

-- TODO lam?
expandExpr (ALam t vs body) = do
    body' <- expandExpr body
    pure $ case body' of
               ALam _ ivs ib -> ALam t (vs++ivs) ib -- Merge the two lambdas
               _             -> ALam t vs body'

-- An app whose type is an arrow is under applied
-- what does this mean when intending to return a lambda?
expandExpr e@(AApp (Forall _ t) f xs) =
    case t of
        TyArr{} -> do
            let tf = let Forall _ tf' = annot f in tf'
            (at, t', vs, args) <- underAppliedToLambda tf xs
            pure $ ALam (Forall [] t') vs (AApp (Forall [] at) f (xs ++ args))
        _       -> pure e

expandExpr (ALet t a b c) =
    -- The outer type *should* still be the same
    ALet t a <$> expandExpr b
             <*> expandExpr c

expandExpr (AUnPrimOp t o a) =
    AUnPrimOp t o <$> expandExpr a

expandExpr (ABinPrimOp t o a b) =
    ABinPrimOp t o <$> expandExpr a
                   <*> expandExpr b

expandExpr (AIfThenElse t pr tr fl) =
    AIfThenElse t <$> expandExpr pr
                  <*> expandExpr tr
                  <*> expandExpr fl

expandExpr AClo{} =
    error "AClo"

expandExpr ACallClo{} =
    error "ACallClo"

underAppliedToLambda :: Eq s => Type s
                             -> [AExpr (Polytype s) ByteString]
                             -> State EtaState (Type s, Type s, [ByteString], [AExpr (Polytype s) ByteString])

underAppliedToLambda at [] = go [] at
    where
    go acc (TyArr a b) = do
        v <- genSym
        go ((v, a):acc) b
    go acc t = do
        let acc' = reverse acc
            vs   = map fst acc'
            args = map (\(v, vt) -> ATerm (Forall [] vt) (Var v)) acc'
        pure (t, at, vs, args)

underAppliedToLambda (TyArr a b) (x:xs) =
    let xt' = let Forall _ xt = annot x in xt
    in if xt' == a
        then underAppliedToLambda b xs
        else error "Type mismatch"

underAppliedToLambda _ _ = error "bad underapply"

functionCallToLambda :: ByteString
                     -> Type s
                     -> State EtaState (AExpr (Polytype s) ByteString)
functionCallToLambda f t' = go [] t'
    where
    go acc (TyArr a b) = genSym >>= \v -> go ((v, a):acc) b
    go acc t =
        let acc' = reverse acc
            args = map (\(v,vt) -> ATerm (Forall [] vt) (Var v)) acc'
            vs   = map fst acc'
            body = AApp (Forall [] t)
                        (ATerm (Forall [] t') $ Var f)
                        args
        in pure $ ALam (Forall [] t') vs body

genSym :: State EtaState ByteString
genSym = do
    EtaState fc <- get
    put $ EtaState $! fc+1
    pure . pack $ "eta_" <> (show fc)
