<<<<<<< HEAD
<<<<<<< HEAD
module Phase.EtaExpand (etaExpand) where

import Common.State
import Core.Definition
import Core.Term
import TypeCheck.TypedExpression
import TypeCheck.Types
=======
=======
>>>>>>> eta expansion
module Phase.EtaExpand where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
<<<<<<< HEAD
>>>>>>> eta expansion
=======
>>>>>>> eta expansion

import Control.Monad         (replicateM)
import Data.ByteString.Char8 (ByteString, pack)

<<<<<<< HEAD
<<<<<<< HEAD
etaExpand :: TypedModule Scheme ByteString -> TypedModule Scheme ByteString
etaExpand md = md { getFunDefnsT = evalState (mapM etaExpand' $ getFunDefnsT md) 0 }

etaExpand' :: FunDefnT Scheme ByteString -> State Int (FunDefnT Scheme ByteString)
etaExpand' (FunDefnT t s e) = FunDefnT t s <$> etaExpandExpr genSym e

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
    pure s

-- TODO check free vars
retype :: Scheme -> [Term s] -> [TypedExpr Scheme s]
retype scheme [x] = [TermT scheme x]
retype (Forall vs (TyArr t1 t2)) (f:xs) =
    TermT (Forall vs t1) f : retype (Forall vs t2) xs
retype _ _ = error "Bad retype input"
=======
=======
>>>>>>> eta expansion
etaExpand :: TypedModule ByteString -> TypedModule ByteString
etaExpand md = md { getTFunDefns = fst $ runState (mapM etaExpand' $ getTFunDefns md) 0 }

etaExpand' :: TFunDefn ByteString -> State Int (TFunDefn ByteString)
etaExpand' (TFunDefn t n e) = do

    (extra, e') <- etaExpandExpr genSym e
    case extra of
        [] -> pure $ TFunDefn t n e'
        _  ->
            let t' = fixType t
            in pure $ TFunDefn t n (ALam t' extra e')

    where
    fixType (Forall _ t') = t' -- Guess.  Needs proper instantiation?

    genSym :: State Int ByteString
    genSym = do
        i <- get
        let s = pack $ "e" <> show i
        put $! i + 1
        pure s



-- Eta-expansion only makes sense if there's an enclosing lambda to expand
data EnclosedBy = NonLambda
                | Lambda

data Applied s = Applied (Type s)
               | UnderApplied [Type s]
               | OverApplied

etaExpandExpr :: State Int s
              -> AExpr (Type s) s
              -> State Int ([s], AExpr (Type s) s)

etaExpandExpr genSym = go Lambda
    where
    go enclosedBy at@(ATerm t _) =

        case enclosedBy of

            NonLambda ->
                pure ([], at)

            -- Can only eta-expand if surrounded by lambda
            Lambda -> do

                -- Find the types of the parameters to generate
                let extraTypes =
                        case applyTypes t [] of
                            Applied _       -> []
                            UnderApplied xs -> xs
                            OverApplied     -> error "can't happen"

                -- Generate names for them
                extraTerms <- replicateM (length extraTypes) genSym

                let extraTypedTerms = zipWith (\ty te -> ATerm ty (Var te)) extraTypes extraTerms

                -- Fix the type of the application, now that it's applied
                let appType = let Applied ty = applyTypes t extraTypes in ty

                -- Tell the surrounding lambda the names of the terms
                pure (extraTerms, AApp appType at extraTypedTerms)

    go _ (ALam t vs body) = do
        (extraTerms, body') <- go Lambda body
        pure ([], ALam t (vs ++ extraTerms) body')

    go enclosedBy (AApp t f xs) = do
        ([], f' ) <- go NonLambda f
        ([], xs') <- (\(a, b) -> (concat a,b)) . unzip <$> mapM (go NonLambda) xs

        -- Check for under-applications (enclosed by a lambda)
        case enclosedBy of

            NonLambda ->
                pure ([], AApp t f' xs')

            Lambda -> do

                -- Find the types of the parameters to generate
                let extraTypes =
                        case applyTypes (annot f') (map annot xs') of
                            Applied _       -> []
                            UnderApplied ua -> ua
                            OverApplied     -> error "can't happen"

                                -- Generate names for them
                extraTerms <- replicateM (length extraTypes) genSym

                let extraTypedTerms = zipWith (\ty te -> ATerm ty (Var te)) extraTypes extraTerms

                -- Fix the type of the application, now that it's applied
                let appType = let Applied ty = applyTypes t extraTypes in ty

                pure (extraTerms, AApp appType f' (xs' ++ extraTypedTerms))

    go _ (ALet t a b c) = do
        ([], b') <- go NonLambda b
        ([], c') <- go NonLambda c
        pure ([], ALet t a b' c')

    go _ (AUnPrimOp t o e) = do
        ([], e') <- go NonLambda e
        pure ([], AUnPrimOp t o e')

    go _ (ABinPrimOp t o e1 e2) = do
        ([], e1') <- go NonLambda e1
        ([], e2') <- go NonLambda e2
        pure ([], ABinPrimOp t o e1' e2')

    go _ (AIfThenElse ty p t f) = do
        ([], p') <- go NonLambda p
        ([], t') <- go NonLambda t
        ([], f') <- go NonLambda f
        pure ([], AIfThenElse ty p' t' f')

    go _ AClo{} =
        error "closure"

    go _ ACallClo{} =
        error "call closure"

applyTypes :: Type s -> [Type s] -> Applied s
applyTypes = go
    where
    go t@(TyArr _ b) xs =
        case xs of
            (_:xs') -> go b xs'
            []      -> underApplied [] t
    go t [] = Applied t
    go _ _  = OverApplied

    underApplied acc (TyArr a b) = underApplied (a:acc) b
    underApplied acc           _ = UnderApplied $ reverse acc
<<<<<<< HEAD
>>>>>>> eta expansion
=======
>>>>>>> eta expansion
