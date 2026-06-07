{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module Phase.Anf.AnfModule ( AnfModule (..)
                           , FunDefAnfT (..)
                           , anfModule ) where

import Common.EitherT
import Common.State
import Common.Trans
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Phase.Anf.AnfExpression

import           Data.ByteString.Char8   (ByteString, pack)
import           Data.Functor            ((<&>))
import           Data.Set                (Set)
import qualified Data.Set as S


data AnfModule s =
    AnfModule { getDataDefnAnfTs :: [DataDefn s]
              , getFunDefAnfTs   :: [FunDefAnfT s]
              } deriving Show

data AnfState s =
    AnfState { getNum    :: !Int
             , symGen    :: !(State (AnfState s) s)
             , localRefs :: !(Set s)
             }

type Anf s a =
    EitherT ByteString (
        State (AnfState s)) a

data FunDefAnfT s =
    FunDefAnfT s (Quant s) (NExp s) -- Maybe this should be forced into a Lambda
        deriving Show

anfModule :: Module (Type ByteString) ByteString
          -> Either ByteString (AnfModule ByteString)
anfModule modu = do
    let funDefs = getFunDefns modu
    funDefs' <- mapM anfFunDefT funDefs
    pure $ AnfModule (getDataDefns modu) funDefs'

    where
    anfFunDefT (FunDefn n pt expr) = do
        funDef' <- evalState' (AnfState 0 genSym mempty)
                 . runEitherT
                 $ norm expr
        pure $ FunDefAnfT n pt funDef'

genSym :: State (AnfState s) ByteString
genSym = do
    AnfState n sg lrs <- get
    put $! AnfState (n+1) sg lrs
    pure . pack $ "anf_" <> show n

norm :: (Ord s, Show s) => Expr (Type s) s -> Anf s (NExp s)
norm expr = normExpr expr pure

trackingRefs :: (Ord s, Show s) => [s] -> Anf s a -> Anf s a
trackingRefs vs f = do
    refs <- preserveLocalReferences
    registerReferences vs
    y <- f
    restoreLocalReferences refs
    pure y

    where
    registerReferences vs = lift . modify' $ \s -> s { localRefs = S.union (localRefs s) (S.fromList vs)}
    preserveLocalReferences =  lift $ localRefs <$> get
    restoreLocalReferences refs = lift . modify' $ \s -> s { localRefs = refs }

normExpr :: (Ord s, Show s) => Expr (Type s) s
                            -> (NExp s -> Anf s (NExp s))
                            -> Anf s (NExp s)
normExpr expr k =

    case expr of

        App t f xs ->
            normAtom f $ \f' ->
                normAtoms xs $ \xs' ->
                    k $ CExp $ CApp t f' xs'

        Lam t vs body -> do
            body' <- trackingRefs vs (norm body)
            k $ AExp (ALam t vs body')

        Let _ a b c ->
            -- I don't think 'a' is scoped in b (recursive-let)
            normExpr b $ \b' ->
                NLet a b' <$> trackingRefs [a] (normExpr c k)

        IfThenElse t pr tr fl ->
            normAtom pr $ \pr' -> do
                tr' <- norm tr
                fl' <- norm fl
                k $ CExp $ CIfThenElse t pr' tr' fl'

        UnPrimOp t op a ->
            normAtom a $ \a' ->
                k $ AExp $ AUnPrimOp t op a'

        BinPrimOp t op a b ->
            normAtom a $ \a' ->
                normAtom b $ \b' ->
                    k $ AExp $ ABinPrimOp t op a' b'

        Term t (LitBool i) ->
            k $ AExp $ ATerm t $ LitBool i

        Term t (LitInt i) ->
            k $ AExp $ ATerm t $ LitInt i

        Term t (LitString s) ->
            k $ AExp $ ATerm t (LitString s)

        Term t (Var v) -> do

            let term' = ATerm t (Var v)

            nexp <- isCallCandidate t v <&> \case
                        -- Local vars remain as terms
                        False -> AExp term'
                        -- Top-levels turn into complex App nodes
                        True -> CExp $ CApp t term' []

            k nexp

        Term t (DCons d) ->
            k $ AExp $ ATerm t $ DCons d

        -- Probably the same way as IfThenElse !
        Case t scrut ps ->
            normAtom scrut $ \scrut' -> do
                ps' <- mapM normPattern ps
                k $ CExp $ CCase t scrut' ps'

    where
    -- Hacky (and duplicate way) to try to choose between local var and fun call
    isCallCandidate t v = do
        lrs <- lift $ localRefs <$> get
        let isLocal = v `S.member` lrs
        pure $ not isLocal && isSimple t

        where
        isSimple TyArr{} = False
        isSimple       _ = True

-- both parts necessary?
-- assume lhs is already normed for now
normPattern :: (Ord s, Show s) => Pattern (Type s) s -> Anf s (PExp s)
normPattern (Pattern a b) =
    PExp <$> normLhs a <*> norm b

normLhs :: Show s => Expr (Type s) s -> Anf s (PPat s)
normLhs (App t dc ts) = PApp <$> expectDCons dc
                             <*> pure t 
                             <*> mapM expectVar ts
    where
    expectVar (Term _ v@Var{}) = pure v
    expectVar x = left $ "Expected Var: " <> pack (show x)

-- Route just-a-term into a Pattern Apply on 0 params
normLhs dc@(Term t DCons{}) = normLhs (App t dc [])

expectDCons (Term _ (DCons dc)) = pure dc
expectDCons x = left $ "Expected DCons: " <> pack (show x)

normAtom :: (Ord s, Show s) => Expr (Type s) s
                            -> (AExp s -> Anf s (NExp s))
                            -> Anf s (NExp s)
normAtom e k =

    case e of

        -- Assumes v == lam == lam'
        Lam t vs body ->
            normExpr body $ \body' -> do
                v    <- lift (symGen =<< get)
                rest <- k $ ATerm t $ Var v
                pure $ NLet v
                            (AExp $ ALam t vs body')
                            rest

        -- assumes v == app == app'
        App t f xs ->
            normAtom f $ \f' ->
                normAtoms xs $ \xs' -> do
                    v    <- lift (symGen =<< get)
                    rest <- k $ ATerm t $ Var v
                    pure $ NLet v
                                (CExp $ CApp t f' xs')
                                rest

        Let _ a b c ->
            normExpr b $ \b' ->
                NLet a b' <$> normAtom c k

        -- assumes v == ite == ite'
        IfThenElse t pr tr fl ->
            normAtom pr $ \pr' -> do
                v    <- lift (symGen =<< get)
                tr'  <- norm tr
                fl'  <- norm fl
                rest <- k $ ATerm t $ Var v
                pure $ NLet v
                            (CExp $ CIfThenElse t pr' tr' fl')
                            rest

        UnPrimOp t op a ->
            normAtom a $ \a' ->
                k $ AUnPrimOp t op a'

        BinPrimOp t op a b ->
            normAtom a $ \a' ->
                normAtom b $ \b' ->
                    k $ ABinPrimOp t op a' b'

        Term t (LitBool i) ->
            k $ ATerm t (LitBool i)

        Term t (LitInt i) ->
            k $ ATerm t (LitInt i)

        Term t (LitString s) ->
            k $ ATerm t (LitString s)

        Term t (Var v) ->

            let term' = ATerm t (Var v)

            in isCallCandidate t v >>= \case

                   False -> k term'

                   True -> do
                       v    <- lift (symGen =<< get)
                       rest <- trackingRefs [v] (k $ ATerm t $ Var v)
                       pure $ NLet v (CExp (CApp t term' [])) rest

        Term t (DCons d) ->
            k $ ATerm t (DCons d)

    where
    -- Hacky (and duplicate way) to try to choose between local var and fun call
    isCallCandidate t v = do
        lrs <- lift $ localRefs <$> get
        let isLocal = v `S.member` lrs
        pure $ not isLocal && isSimple t

        where
        isSimple TyArr{} = False
        isSimple       _ = True

normAtoms :: (Ord s, Show s) => [Expr (Type s) s]
                             -> ([AExp s] -> Anf s (NExp s))
                             -> Anf s (NExp s)
normAtoms [] k = k []
normAtoms (e:es) k =
    normAtom e $ \e' ->
        normAtoms es $ \es' ->
            k (e':es')
