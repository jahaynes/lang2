{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Phase.Anf.AnfTransform ( anfModule ) where

import Common.StateT
import Common.Trans
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Phase.Anf.Anf
import Phase.Anf.FreeVars

import Control.Monad         (forM)
import Data.ByteString.Char8 (ByteString, pack)
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Anf a =
    StateT (AnfState ByteString) (
        Either ByteString) a

-- May be able to make this infallible
anfModule :: Module (Type ByteString) ByteString
          -> Either ByteString (AnfModule ByteString)
anfModule md = do

    -- Each top-level lambda may yield additional lifted functions.
    -- Use a single state across all function definitions so that
    -- generated labels (ll_0, anf_1, …) are globally unique.
    let state = AnfState { getNum = 0
                         , lifted = mempty
                         , cloTracker = mempty
                         }
    (fundefs, finalState) <- runStateT (mapM anfFunDefT (getFunDefns md)) state

    -- Collect all lifted functions accumulated during the transform.
    let liftedFundefs = map snd . M.toList $ lifted finalState
    pure $ AnfModule (getDataDefns md) (liftedFundefs <> fundefs)

anfFunDefT :: FunDefn (Type ByteString) ByteString
           -> Anf (FunDefAnfT ByteString)
anfFunDefT (FunDefn n pt expr) =

    case expr of

        -- Avoid lambda-lifting functions already on the top-level
        Lam t vs body -> do
            body' <- norm body
            pure $ FunDefAnfT n pt t [] vs body'   -- TODO q vars

        _nonlambda -> do
            expr' <- norm expr
            pure $ FunDefAnfT n pt (typeOf expr) [] [] expr'   -- TODO q vars

data AnfState s =
    AnfState { getNum :: !Int
             , lifted :: !(Map s (FunDefAnfT s))
             , cloTracker :: !(Map s (s, AClosEnv s))
             }

genAnf :: Anf ByteString
genAnf = genSym "anf_"

genLam ::  Anf ByteString
genLam = genSym "ll_"

genSym :: ByteString -> Anf ByteString
genSym pre = do
    AnfState n sg ct <- get
    put $! AnfState (n+1) sg ct
    pure (pre <> (pack $ show n))

norm :: Expr (Type ByteString) ByteString -> Anf (NExp ByteString)
norm expr = asAnfExpr expr pure

asAnfExpr :: Expr (Type ByteString) ByteString
          -> (NExp ByteString -> Anf (NExp ByteString))
          -> Anf (NExp ByteString)
asAnfExpr expr k =

    case expr of

        Term t term ->
            k (AExp $ ATerm t term)

        Lam t vs body -> do
            name  <- genLam
            body' <- norm body

            let free = S.toList $ functionFreeVars vs body'

            if null free

                then do
                    -- This is a lambda (no free vars)
                    let ll = FunDefAnfT name QTodo t [] vs body'
                    modify $ \s -> s { lifted = M.insert name ll (lifted s) }
                    k (AExp $ ATerm t (Var name))
                else do
                    -- This is a closure (has free vars)
                    let ll = FunDefAnfT name QTodo t free vs body'
                    modify $ \s -> s { lifted = M.insert name ll (lifted s) }
                    s <- genAnf
                    let cloEnv = AClosEnv free
                    modify $ \st -> st { cloTracker = M.insert s (name, cloEnv) (cloTracker st) }
                    NLet t s (CExp $ CAppClo t (ATerm t (Var name)) cloEnv []) <$> k (AExp $ ATerm t (Var s))


        App t f xs ->
            case f of
                Lam lam_t vs body -> do
                    name  <- genLam
                    body' <- norm body
                    let free = S.toList $ functionFreeVars vs body'
                    let ll = FunDefAnfT name QTodo lam_t free vs body'
                    modify $ \s -> s { lifted = M.insert name ll (lifted s) }
                    asAtomicExprs xs $ \xs' ->
                        if null free
                            then k (CExp $ CApp t (ATerm lam_t (Var name)) xs')
                            else k (CExp $ CAppClo t (ATerm lam_t (Var name)) (AClosEnv free) xs')
                _ ->
                    asAtomicExpr f $ \f' ->
                        case f' of
                            ATerm _ (Var name) -> do
                                AnfState _ liftedMap cloTrackerMap <- get
                                case M.lookup name liftedMap of
                                    Just (FunDefAnfT _ _ _ env _ _) | not (null env) ->
                                        asAtomicExprs xs $ \xs' ->
                                            k (CExp $ CAppClo t f' (AClosEnv env) xs')
                                    _ ->
                                        case M.lookup name cloTrackerMap of
                                            Just (funcName, cloEnv) ->
                                                asAtomicExprs xs $ \xs' ->
                                                    k (CExp $ CAppClo t (ATerm t (Var funcName)) cloEnv xs')
                                            Nothing ->
                                                asAtomicExprs xs $ \xs' ->
                                                    k (CExp $ CApp t f' xs')
                            _ ->
                                asAtomicExprs xs $ \xs' ->
                                    k (CExp $ CApp t f' xs')

        Let t a b c ->
            asAnfExpr b $ \b' ->
                NLet t a b' <$> asAnfExpr c k

        UnPrimOp t op a ->
            asAtomicExpr a $ \a' ->
                k (CExp $ CUnPrimOp t op a')

        BinPrimOp t op a b ->
            asAtomicExpr a $ \a' ->
                asAtomicExpr b $ \b' ->
                    k (CExp $ CBinPrimOp t op a' b')

        IfThenElse t pr tr fl ->
            asAtomicExpr pr $ \pr' -> do
                v    <- genAnf
                tr'  <- norm tr
                fl'  <- norm fl
                rest <- k (AExp $ ATerm t $ Var v)
                pure $ NLet t v (CExp $ CIfThenElse t pr' tr' fl') rest

        Case t scr ps ->
            asAtomicExpr scr $ \scr' -> do
                v   <- genAnf
                ps' <- forM ps $ \(Pattern pat rhs) -> do
                    rhs' <- norm rhs
                    pure (PExp (check pat) rhs')
                rest <- k (AExp $ ATerm t $ Var v)
                pure $ NLet t v (CExp $ CCase t scr' ps') rest

-- Crap:
--check :: Expr (Type s) s -> PPat s
check e =
    case e of
        Term t (Var v) -> PVar v
        App t f xs -> PApp (c1 f) t (map c2 xs)

    where
    c1 (Term t (Var v) ) = v
    c1 (Term t (DCons dc)) = dc
    c2 (Term t v ) = v

asAtomicExpr :: Expr (Type ByteString) ByteString
             -> (AExp ByteString -> Anf (NExp ByteString))
             -> Anf (NExp ByteString)
asAtomicExpr expr k =

    case expr of

        Term t term ->
            k (ATerm t term)

        Lam t vs body -> do
            name  <- genLam
            body' <- norm body

            let free = S.toList $ functionFreeVars vs body'

            if null free
                then do
                    let ll = FunDefAnfT name QTodo t [] vs body'
                    modify $ \s -> s { lifted = M.insert name ll (lifted s) }
                    k (ATerm t (Var name))
                else do
                    let ll = FunDefAnfT name QTodo t free vs body'
                    modify $ \s -> s { lifted = M.insert name ll (lifted s) }
                    s <- genAnf
                    let cloEnv = AClosEnv free
                    modify $ \st -> st { cloTracker = M.insert s (name, cloEnv) (cloTracker st) }
                    NLet t s (CExp $ CAppClo t (ATerm t (Var name)) cloEnv []) <$> k (ATerm t (Var s))

        App t f xs ->
            case f of
                Lam lam_t vs body -> do
                    name  <- genLam
                    body' <- norm body
                    let free = S.toList $ functionFreeVars vs body'
                    let ll = FunDefAnfT name QTodo lam_t free vs body'
                    modify $ \s -> s { lifted = M.insert name ll (lifted s) }
                    asAtomicExprs xs $ \xs' -> do
                        s <- genAnf
                        let app = if null free
                                    then CExp $ CApp t (ATerm lam_t (Var name)) xs'
                                    else CExp $ CAppClo t (ATerm lam_t (Var name)) (AClosEnv free) xs'
                        NLet t s app <$> k (ATerm t (Var s))
                _ ->
                    asAtomicExpr f $ \f' ->
                        case f' of
                            ATerm _ (Var name) -> do
                                AnfState _ liftedMap cloTrackerMap <- get
                                case M.lookup name liftedMap of
                                    Just (FunDefAnfT _ _ _ env _ _) | not (null env) ->
                                        asAtomicExprs xs $ \xs' -> do
                                            s <- genAnf
                                            NLet t s (CExp $ CAppClo t f' (AClosEnv env) xs') <$> k (ATerm t (Var s))
                                    _ ->
                                        case M.lookup name cloTrackerMap of
                                            Just (funcName, cloEnv) ->
                                                asAtomicExprs xs $ \xs' -> do
                                                    s <- genAnf
                                                    NLet t s (CExp $ CAppClo t (ATerm t (Var funcName)) cloEnv xs') <$> k (ATerm t (Var s))
                                            Nothing ->
                                                asAtomicExprs xs $ \xs' -> do
                                                    s <- genAnf
                                                    NLet t s (CExp $ CApp t f' xs') <$> k (ATerm t (Var s))
                            _ ->
                                asAtomicExprs xs $ \xs' -> do
                                    s <- genAnf
                                    NLet t s (CExp $ CApp t f' xs') <$> k (ATerm t (Var s))

        Let t a b c ->
            asAnfExpr b $ \b' ->
                NLet t a b' <$> asAtomicExpr c k

        UnPrimOp t op a ->
            asAtomicExpr a $ \a' -> do
                s    <- genAnf
                rest <- k (ATerm t (Var s))
                pure $ NLet t s (CExp $ CUnPrimOp t op a') rest

        BinPrimOp t op a b ->
            asAtomicExpr a $ \a' ->
                asAtomicExpr b $ \b' -> do
                    s    <- genAnf
                    rest <- k (ATerm t (Var s))
                    pure $ NLet t s (CExp $ CBinPrimOp t op a' b') rest

        IfThenElse t pr tr fl ->
            asAtomicExpr pr $ \pr' -> do
                v    <- genAnf
                tr'  <- norm tr
                fl'  <- norm fl
                rest <- k (ATerm t $ Var v)
                pure $ NLet t v (CExp $ CIfThenElse t pr' tr' fl') rest

        Case t scr ps ->
            asAtomicExpr scr $ \scr' -> do
                ps' <- forM ps $ \(Pattern pat rhs) ->
                    PExp (check pat) <$> asAnfExpr rhs pure
                v    <- genAnf
                rest <- k (ATerm t (Var v))
                pure $ NLet t v (CExp $ CCase t scr' ps') rest

asAtomicExprs :: [Expr (Type ByteString) ByteString]
              -> ([AExp ByteString] -> Anf  (NExp ByteString))
              -> Anf (NExp ByteString)
asAtomicExprs [] k = k []
asAtomicExprs (e:es) k =
    asAtomicExpr e $ \e' ->
        asAtomicExprs es $ \es' ->
            k (e':es')

{-

normExpr :: Show s => Expr (Type s) s
                   -> (NExp s -> Anf s (NExp s))
                   -> Anf s (NExp s)
normExpr expr k =

    case expr of

        App t f xs ->
            normAtom f $ \f' ->
                normAtoms xs $ \xs' ->
                    k $ CExp $ CApp t f' xs'

        Lam t vs body -> do
            body' <- norm body
            k $ AExp (ALam t vs body')

        Let _ a b c ->
            normExpr b $ \b' ->
                NLet a b' <$> normExpr c k

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

        Term t (Var v) ->
            k $ AExp $ ATerm t $ Var v

        Term t (DCons d) ->
            k $ AExp $ ATerm t $ DCons d

        -- Probably the same way as IfThenElse !
        Case t scrut ps ->
            normAtom scrut $ \scrut' -> do
                ps' <- mapM normPattern ps
                k $ CExp $ CCase t scrut' ps'

-- both parts necessary?
-- assume lhs is already normed for now
normPattern :: Show s => Pattern (Type s) s -> Anf s (PExp s)
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

normAtom :: Show s => Expr (Type s) s
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
            k $ ATerm t (Var v)

        Term t (DCons d) ->
            k $ ATerm t (DCons d)

normAtoms :: Show s => [Expr (Type s) s]
                    -> ([AExp s] -> Anf s (NExp s))
                    -> Anf s (NExp s)
normAtoms [] k = k []
normAtoms (e:es) k =
    normAtom e $ \e' ->
        normAtoms es $ \es' ->
            k (e':es')

-}
