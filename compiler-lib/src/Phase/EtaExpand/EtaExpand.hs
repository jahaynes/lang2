{-# LANGUAGE LambdaCase #-}

module Phase.EtaExpand.EtaExpand (EtaState (..), etaExpand, expandDefn) where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Phase.EtaExpand.EtaSaturate

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Functor          ((<&>))
import           Data.List             ((\\))
import           Data.Map.Strict       ((!), Map)
import qualified Data.Map.Strict as M

data EtaState =
    EtaState { getFreshCount  :: !Int
             , getKnownTypes  :: !(Map ByteString (Type ByteString))
             , getExtraParams :: !(Map ByteString [(ByteString, Type ByteString)])
             } deriving Show

-- TODO can just be t?
etaExpand :: Module (Type ByteString) ByteString
          -> Module (Type ByteString) ByteString
etaExpand md =

    let (funDefns', st) = runState (mapM expandDefn $ getFunDefns md) (EtaState 0 mempty mempty)

    in etaSaturate (md { getFunDefns = funDefns' })
                   (getExtraParams st)

expandDefn :: FunDefn (Type ByteString) ByteString
           -> State EtaState (FunDefn (Type ByteString) ByteString)
expandDefn (FunDefn n q e) = do

    e' <- expandExpr e

    case (e, e') of
        (Lam _ vs _, Lam _ vs' _) ->
            case vs' \\ vs of
                [] -> pure ()
                vd ->
                    modify' $ \st ->
                        let kt  = getKnownTypes st
                            -- TODO could prevent re-use of 'v' here (if alphabetisation needed)
                            tvd = map (\v -> (v, kt ! v)) vd
                        in st { getExtraParams = M.insert n tvd (getExtraParams st) }
        _ -> pure ()

    pure $ FunDefn n q e'

expandExpr :: Expr (Type ByteString) ByteString
           -> State EtaState (Expr (Type ByteString) ByteString)
expandExpr e@(Term t term) =
    case (t, term) of
        (TyArr{}, Var f)   -> functionCallToLambda f t
        (TyArr{}, DCons _) -> error "Not implemented: eta expansion of data constructors"
        (TyArr{},     _)   -> error "Unexpected type arrow for non-var term"
        _                  -> pure e

-- TODO lam?
expandExpr (Lam t vs body) =
    expandExpr body <&> \case
        Lam _ ivs ib -> Lam t (vs++ivs) ib -- Merge the two lambdas
        body'         -> Lam t vs body'

-- An app whose type is an arrow is under applied
-- what does this mean when intending to return a lambda?
expandExpr e@(App t f xs) =
    case t of
        TyArr{} -> do
            let tf = typeOf f
            (at, t', vs, args) <- underAppliedToLambda tf xs
            pure $ Lam t' vs (App at f (xs ++ args))
        _       -> pure e

expandExpr (Let t a b c) =
    -- The outer type *should* still be the same
    Let t a <$> expandExpr b
             <*> expandExpr c

expandExpr (UnPrimOp t o a) =
    UnPrimOp t o <$> expandExpr a

expandExpr (BinPrimOp t o a b) =
    BinPrimOp t o <$> expandExpr a
                   <*> expandExpr b

expandExpr (IfThenElse t pr tr fl) =
    IfThenElse t <$> expandExpr pr
                  <*> expandExpr tr
                  <*> expandExpr fl

expandExpr (Case t scrut ps) =
    Case t <$> expandExpr scrut
            <*> mapM expandPat ps

-- necessary?
expandPat :: Pattern (Type ByteString) ByteString
          -> State EtaState (Pattern (Type ByteString) ByteString)
expandPat (Pattern a b) =
    Pattern <$> expandExpr a <*> expandExpr b

underAppliedToLambda :: Type ByteString
                     -> [Expr (Type ByteString) ByteString]
                     -> State EtaState ( Type ByteString
                                       , Type ByteString
                                       , [ByteString]
                                       , [Expr (Type ByteString) ByteString] )

underAppliedToLambda at [] = go [] at
    where
    go acc (TyArr a b) = do
        v <- genTypedSym a
        go ((v, a):acc) b
    go acc t = do
        let acc' = reverse acc
            vs   = map fst acc'
            args = map (\(v, vt) -> Term vt (Var v)) acc'
        pure (t, at, vs, args)

underAppliedToLambda (TyArr a b) (x:xs) =
    let xt' = typeOf x
    in if xt' == a
        then underAppliedToLambda b xs
        else error "Type mismatch"

underAppliedToLambda _ _ = error "bad underapply"

functionCallToLambda :: ByteString
                     -> Type ByteString
                     -> State EtaState (Expr (Type ByteString) ByteString)
functionCallToLambda f t' = go [] t'
    where
    go acc (TyArr a b) = do
        v <- genTypedSym a
        go ((v, a):acc) b
    go acc t =
        let acc' = reverse acc
            args = map (\(v,vt) -> Term vt (Var v)) acc'
            vs   = map fst acc'
            body = App t
                        (Term t' $ Var f)
                        args
        in pure $ Lam t' vs body

genTypedSym :: Type ByteString -> State EtaState ByteString
genTypedSym t = do
    EtaState fc kt ep <- get
    let v   = pack $ "eta_" <> show fc
        kt' = M.insert v t kt
    put $ EtaState (fc+1) kt' ep
    pure v