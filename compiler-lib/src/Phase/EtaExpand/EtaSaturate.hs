module Phase.EtaExpand.EtaSaturate (etaSaturate) where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types

import           Control.Monad         (replicateM)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe            (fromMaybe)
import           Debug.Trace           (trace)

newtype SatState =
    SatState { getFreshCount :: Int }

etaSaturate :: ModuleT ByteString
            -> Map ByteString [(ByteString, Type ByteString)]
            -> ModuleT ByteString
etaSaturate md extraParams = do

    let funDefns = getFunDefnTs md

    let funDefns' = evalState (mapM saturate' funDefns) (SatState 0)

    md { getFunDefnTs = funDefns' }

    where
    saturate' (FunDefnT t n e) = FunDefnT t n <$> saturateExpr genSym extraParams e

genSym :: State SatState ByteString
genSym = do
    SatState fc <- get
    put $ SatState $! fc+1
    pure . pack $ "ate_" <> show fc

saturateExpr :: (Ord s, Show s) => State SatState s
                                -> Map s [(s, Type s)]
                                -> ExprT s
                                -> State SatState (ExprT s)
saturateExpr symGen extraParams exprT = go exprT

    where
    go (TermT t term) =
        pure $ TermT t term

    go (LamT t vs body) =
        LamT t vs <$> go body

    go (AppT t f@(TermT _ (Var fv)) xs) = do

        f'  <- go f
        xs' <- mapM go xs

        pure $ case M.lookup fv extraParams of

            Nothing ->
                AppT t f' xs'

            Just eps -> do

                -- Append the extra (typed) arguments to the apply
                let xs'' = xs' ++ map (\(v, vt) -> TermT vt (Var v)) eps

                -- The extra variables for the fresh enclosing lambda
                let vs = map fst eps

                -- Calculate the type of the fresh enclosing lambda
                -- TODO untested
                let t' = foldr TyArr t (map snd eps)

                LamT t' vs (AppT t f' xs'')

    go (AppT t f xs) =
        AppT t <$> go f <*> mapM go xs

    go (LetT t a b c) =
        LetT t a <$> go b <*> go c

    go (UnPrimOpT t op e1) =
        UnPrimOpT t op <$> go e1

    go (BinPrimOpT t op e1 e2) =
        BinPrimOpT t op <$> go e1 <*> go e2

    go (IfThenElseT t pr tr fl) =
        IfThenElseT t <$> go pr <*> go tr <*> go fl

-- Is this alpha-reduction -- reuse?
substitute :: Ord s => [s] -> ExprT s -> [ExprT s] -> ExprT s
substitute vs' body' xs' = go (M.fromList $ zip vs' xs') body'

    where
    go subst term@(TermT _ (Var v)) =
          fromMaybe term (M.lookup v subst)

    go _ t@TermT{} = t

    -- TODO is this enough to prevent variable capture
    go subst (LamT t vs body) =
        let subst' = foldr M.delete subst vs
        in LamT t vs (go subst' body)

    go subst (AppT t f xs) =
        AppT t (go subst f) (map (go subst) xs)

    go subst (LetT t a b c) =
        let subst' = M.delete a subst
        in LetT t a (go subst' b) (go subst' c)

    go subst (UnPrimOpT t op e1) =
        UnPrimOpT t op (go subst e1)

    go subst (BinPrimOpT t op e1 e2) =
        BinPrimOpT t op (go subst e1) (go subst e2)

    go subst (IfThenElseT t p tr f) =
        IfThenElseT t (go subst p) (go subst tr) (go subst f)
