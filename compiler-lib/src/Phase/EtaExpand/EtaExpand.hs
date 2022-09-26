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

etaExpand :: ModuleT ByteString -> ModuleT ByteString
etaExpand md =

    let (funDefns', st) = runState (mapM expandDefn $ getFunDefnTs md) (EtaState 0 mempty mempty)

    in etaSaturate (md { getFunDefnTs = funDefns' })
                   (getExtraParams st)

expandDefn :: FunDefnT ByteString
           -> State EtaState (FunDefnT ByteString)
expandDefn (FunDefnT n q e) = do

    e' <- expandExpr e

    case (e, e') of
        (LamT _ vs _, LamT _ vs' _) ->
            case vs' \\ vs of
                [] -> pure ()
                vd ->
                    modify' $ \st ->
                        let kt  = getKnownTypes st
                            -- TODO could prevent re-use of 'v' here (if alphabetisation needed)
                            tvd = map (\v -> (v, kt ! v)) vd
                        in st { getExtraParams = M.insert n tvd (getExtraParams st) }
        _ -> pure ()

    pure $ FunDefnT n q e'

expandExpr :: ExprT ByteString
           -> State EtaState (ExprT ByteString)
expandExpr e@(TermT t term) =
    case (t, term) of
        (TyArr{}, Var f)   -> functionCallToLambda f t
        (TyArr{}, DCons _) -> error "Not implemented: eta expansion of data constructors"
        (TyArr{},     _)   -> error "Unexpected type arrow for non-var term"
        _                  -> pure e

-- TODO lam?
expandExpr (LamT t vs body) =
    expandExpr body <&> \case
        LamT _ ivs ib -> LamT t (vs++ivs) ib -- Merge the two lambdas
        body'         -> LamT t vs body'

-- An app whose type is an arrow is under applied
-- what does this mean when intending to return a lambda?
expandExpr e@(AppT t f xs) =
    case t of
        TyArr{} -> do
            let tf = typeOf f
            (at, t', vs, args) <- underAppliedToLambda tf xs
            pure $ LamT t' vs (AppT at f (xs ++ args))
        _       -> pure e

expandExpr (LetT t a b c) =
    -- The outer type *should* still be the same
    LetT t a <$> expandExpr b
             <*> expandExpr c

expandExpr (UnPrimOpT t o a) =
    UnPrimOpT t o <$> expandExpr a

expandExpr (BinPrimOpT t o a b) =
    BinPrimOpT t o <$> expandExpr a
                   <*> expandExpr b

expandExpr (IfThenElseT t pr tr fl) =
    IfThenElseT t <$> expandExpr pr
                  <*> expandExpr tr
                  <*> expandExpr fl

underAppliedToLambda :: Type ByteString
                     -> [ExprT ByteString]
                     -> State EtaState ( Type ByteString
                                       , Type ByteString
                                       , [ByteString]
                                       , [ExprT ByteString] )

underAppliedToLambda at [] = go [] at
    where
    go acc (TyArr a b) = do
        v <- genTypedSym a
        go ((v, a):acc) b
    go acc t = do
        let acc' = reverse acc
            vs   = map fst acc'
            args = map (\(v, vt) -> TermT vt (Var v)) acc'
        pure (t, at, vs, args)

underAppliedToLambda (TyArr a b) (x:xs) =
    let xt' = typeOf x
    in if xt' == a
        then underAppliedToLambda b xs
        else error "Type mismatch"

underAppliedToLambda _ _ = error "bad underapply"

functionCallToLambda :: ByteString
                     -> Type ByteString
                     -> State EtaState (ExprT ByteString)
functionCallToLambda f t' = go [] t'
    where
    go acc (TyArr a b) = do
        v <- genTypedSym a
        go ((v, a):acc) b
    go acc t =
        let acc' = reverse acc
            args = map (\(v,vt) -> TermT vt (Var v)) acc'
            vs   = map fst acc'
            body = AppT t
                        (TermT t' $ Var f)
                        args
        in pure $ LamT t' vs body

genTypedSym :: Type ByteString -> State EtaState ByteString
genTypedSym t = do
    EtaState fc kt ep <- get
    let v   = pack $ "eta_" <> show fc
        kt' = M.insert v t kt
    put $ EtaState (fc+1) kt' ep
    pure v