{-# LANGUAGE OverloadedStrings #-}

module Phase.Anf.AnfTransform ( anfModule ) where

import           Common.State  (State (runState), modify')
import qualified Common.State as State (get, put)
import           Common.StateT (StateT (runStateT))
import qualified Common.StateT as StateT (get, put)
import           Common.Trans
import           Core.Expression
import           Core.Module
import           Core.Term
import           Core.Types
import           Phase.Anf.Anf hiding (PPat(..))
import qualified Phase.Anf.Anf as Anf (PPat(..))

import           Control.Monad         (forM)
import           Data.ByteString.Char8 (ByteString, pack)

type Anf a =
    StateT (AnfState ByteString) (
        Either ByteString) a

type Ll a =
    State (LlState ByteString) a

-- May be able to make this infallible
anfModule :: Module (Type ByteString) ByteString
          -> Either ByteString (AnfModule ByteString)
anfModule md = do

    -- Lambda pass
    let (topLevelFunDevs, lambdaState) = runState (mapM liftFun (getFunDefns md)) (LlState 0 mempty)

    -- Anf pass
    (anfDefns, _) <- runStateT (mapM anfFunDefT (topLevelFunDevs <> lifted lambdaState)) (AnfState 0)

    pure $ AnfModule (getDataDefns md) anfDefns

anfFunDefT :: FunDefn (Type ByteString) ByteString
           -> Anf (FunDefAnfT ByteString)
anfFunDefT (FunDefn n pt expr) =

    -- The top level is the only place we hit lambdas now
    case expr of

        Lam _t vs body ->
            FunDefAnfT n pt (typeOf expr) [] vs <$> norm body -- TODO q vars

        _nonLambda ->
            FunDefAnfT n pt (typeOf expr) [] [] <$> norm expr -- TODO q vars

newtype AnfState s =
    AnfState { getAnfNum :: Int
             }

data LlState s =
    LlState { getLamNum :: !Int
            , lifted    :: ![FunDefn (Type s) s]
            }

genLam :: Ll ByteString
genLam = do
    s <- State.get
    let n = getLamNum s
    State.put $! s { getLamNum = n+1 }
    pure ("ll_" <> (pack $ show n))

genAnf :: Anf ByteString
genAnf = do
    s <- StateT.get
    let n = getAnfNum s
    StateT.put $! s { getAnfNum = n+1 }
    pure ("anf_" <> (pack $ show n))

liftFun :: FunDefn (Type ByteString) ByteString
        -> Ll (FunDefn (Type ByteString) ByteString)

liftFun (FunDefn name q expr) =
    FunDefn name q <$> liftLambdas expr

liftLambdas :: Expr (Type ByteString) ByteString
            -> Ll (Expr (Type ByteString) ByteString)
liftLambdas = go Nothing

    where
    go mName expr =

        case expr of

            Term{} ->
                pure expr

            Lam t vs body -> do
                to <- genLam
                -- alpha-rename any variables within the lambda to the fresh name (unless shadowed by vs)
                let body' =
                        case mName of
                            Just n | not (n `elem` vs) -> alphaSubstitute n to body
                            _                          -> body
                body'' <- go Nothing body'
                modify' $ \s -> s { lifted = FunDefn to QTodo (Lam t vs body'') : lifted s }
                pure (Term t (Var to))

            App t f xs ->
                App t <$> go Nothing f
                    <*> traverse (go Nothing) xs

            Let t a b c ->
                Let t a <$> go (Just a) b -- Pass a through so lambda self-references can be updated
                        <*> go Nothing c  -- Renaming not needed in c, because the 'Let' stmt does it at runtime

            UnPrimOp t op a ->
                UnPrimOp t op <$> go Nothing a

            BinPrimOp t op a b ->
                BinPrimOp t op <$> go Nothing a
                               <*> go Nothing b

            IfThenElse t pr tr fl ->
                IfThenElse t <$> go Nothing pr
                             <*> go Nothing tr
                             <*> go Nothing fl

            Case t scr ps ->
                let liftLambdasPattern (Pattern lhs rhs) = Pattern lhs <$> go Nothing rhs in
                Case t <$> go Nothing scr
                       <*> traverse liftLambdasPattern ps

norm :: Expr (Type ByteString) ByteString -> Anf (NExp ByteString)
norm expr = asAnfExpr expr pure

asAnfExpr :: Expr (Type ByteString) ByteString
          -> (NExp ByteString -> Anf (NExp ByteString))
          -> Anf (NExp ByteString)
asAnfExpr expr k =

    case expr of

        Term t term ->
            k (AExp $ ATerm t term)

        Lam{} ->
            lift $ Left "asAnfExpr: Cannot construct lambdas in outgoing language"

        App t f xs ->
            asAtomicExpr f $ \f' ->
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

asAnfLhs :: PatLhsExpr (Type s) s -> Anf.PPat s
asAnfLhs (PVar _ v)          = Anf.PVar v
asAnfLhs (PDCons ty dc pats) = Anf.PApp dc ty (map asAnfLhs pats)

asAtomicExpr :: Expr (Type ByteString) ByteString
             -> (AExp ByteString -> Anf (NExp ByteString))
             -> Anf (NExp ByteString)
asAtomicExpr expr k =

    case expr of

        Term t term ->
            k (ATerm t term)

        Lam{} ->
            lift $ Left "asAtomicExpr: Cannot construct lambdas in outgoing language"

        App t f xs ->
            asAtomicExpr f $ \f' ->
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
