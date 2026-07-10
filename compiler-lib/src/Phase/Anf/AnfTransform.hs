{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Phase.Anf.AnfTransform ( anfModule ) where

import Common.StateT
import Common.Trans
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import           Phase.Anf.Anf hiding (PPat(..))
import qualified Phase.Anf.Anf as Anf (PPat(..))
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

    let state = AnfState { getNum = 0
                         , lifted = mempty
                         , cloTracker = mempty
                         }
    (fundefs, finalState) <- runStateT (mapM anfFunDefT (getFunDefns md)) state

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
                    pure (PExp (asAnfLhs pat) rhs')
                rest <- k (AExp $ ATerm t $ Var v)
                pure $ NLet t v (CExp $ CCase t scr' ps') rest

asAnfLhs :: PatLhsExpr (Type ByteString) ByteString -> Anf.PPat ByteString
asAnfLhs (PVar _ v)                = Anf.PVar v
asAnfLhs (PDCons ty dc pats)       = Anf.PApp dc ty (map asAnfLhs pats)

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
                    PExp (asAnfLhs pat) <$> asAnfExpr rhs pure
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
