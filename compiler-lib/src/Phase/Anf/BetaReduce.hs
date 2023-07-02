module Phase.Anf.BetaReduce (betaReduce) where

import Common.State
import Core.Term
import Phase.Anf.AnfExpression

import           Data.Set (Set)
import qualified Data.Set as S

newtype Hygiene s =
    Hygiene { getMasked :: Set s }

-- TODO do occurs-check first?
-- (probably don't want to infinitely substitute)
-- (or maybe the mask already prevents this?)
betaReduce :: Ord s => s -> AExp s -> NExp s -> NExp s
betaReduce a b c = fst $ runState (goNExp c) (Hygiene mempty)

    where
    goAExp (ATerm t term) = goTerm t term
    goAExp (ALam t vs body) = do
        preserved <- get
        mask vs
        body' <- goNExp body
        put preserved
        pure $ ALam t vs body'
    goAExp (AClo t fvs vs body) = do
        preserved <- get
        mask fvs
        mask vs
        body' <- goNExp body
        put preserved
        pure $ AClo t fvs vs body'
    goAExp (AUnPrimOp t op x) = AUnPrimOp t op <$> goAExp x
    goAExp (ABinPrimOp t op x y) = ABinPrimOp t op <$> goAExp x <*> goAExp y
    goAExp AClosEnv{} = error "idk?"

    goCExp (CApp t f xs) = CApp t <$> goAExp f <*> mapM goAExp xs
    goCExp (CIfThenElse t pr tr fl) = CIfThenElse t <$> goAExp pr
                                                    <*> goNExp tr
                                                    <*> goNExp fl
    goCExp CCase{} = error "goCExp CCase"

    goNExp (AExp aexp) = AExp <$> goAExp aexp
    goNExp (CExp cexp) = CExp <$> goCExp cexp
    goNExp (NLet x y z) = do
        preserved <- get
        mask [x]
        y' <- goNExp y
        z' <- goNExp z
        put preserved
        pure $ NLet x y' z'

    goTerm t lb@LitBool{}   = pure (ATerm t lb)
    goTerm t li@LitInt{}    = pure (ATerm t li)
    goTerm t ls@LitString{} = pure (ATerm t ls)
    goTerm t (Var v)
        | v == a = do
            masked <- getMasked <$> get
            pure $ if v `S.member` masked
                       then ATerm t (Var v)
                       else b
        | otherwise = pure $ ATerm t (Var v)
    goTerm _ DCons{} = error "goTerm DCons"

mask :: Ord s => [s] -> State (Hygiene s) ()
mask as = modify' $ \h -> h { getMasked = foldr S.insert (getMasked h) as }
