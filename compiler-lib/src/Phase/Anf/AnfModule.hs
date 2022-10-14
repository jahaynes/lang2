{-# LANGUAGE OverloadedStrings #-}

module Phase.Anf.AnfModule where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Phase.Anf.AnfExpression

import Data.ByteString.Char8 (ByteString, pack)

newtype AnfModule s =
    AnfModule { getFunDefAnfTs :: [FunDefAnfT s]
              } deriving Show

anfModule :: ModuleT ByteString -> AnfModule ByteString
anfModule = AnfModule . map anfFunDefT . getFunDefnTs

data FunDefAnfT s =
    FunDefAnfT s (Quant s) (NExp s)
        deriving Show

anfFunDefT :: FunDefnT ByteString -> FunDefAnfT ByteString
anfFunDefT (FunDefnT n pt expr) =
    let expr' = evalState (norm expr) (AnfState 0 genSym)
    in FunDefAnfT n pt expr'

data AnfState s =
    AnfState { getNum :: Int
             , symGen :: State (AnfState s) s
             }

genSym :: State (AnfState s) ByteString
genSym = do
    AnfState n sg <- get
    put $! AnfState (n+1) sg
    pure . pack $ "anf_" <> show n

norm :: Show s => ExprT s -> State (AnfState s) (NExp s)
norm expr = normExpr expr pure

normExpr :: Show s => ExprT s
                   -> (NExp s -> State (AnfState s) (NExp s))
                   -> State (AnfState s) (NExp s)
normExpr expr k =

    case expr of

        AppT _ f xs ->
            normAtom f $ \f' ->
                normAtoms xs $ \xs' ->
                    k $ CExp $ CApp f' xs'

        LamT t vs body -> do
            body' <- norm body
            k $ AExp (ALam t vs body')

        LetT _ a b c ->
            normExpr b $ \b' ->
                NLet a b' <$> normExpr c k

        IfThenElseT _ pr tr fl ->
            normAtom pr $ \pr' -> do
                tr' <- norm tr
                fl' <- norm fl
                k $ CExp $ CIfThenElse pr' tr' fl'

        UnPrimOpT _ op a ->
            normAtom a $ \a' ->
                k $ AExp $ AUnPrimOp op a'

        BinPrimOpT _ op a b ->
            normAtom a $ \a' ->
                normAtom b $ \b' ->
                    k $ AExp $ ABinPrimOp op a' b'

        TermT t (LitBool i) ->
            k $ AExp $ ATerm t $ LitBool i

        TermT t (LitInt i) ->
            k $ AExp $ ATerm t $ LitInt i

        TermT t (LitString s) ->
            k $ AExp $ ATerm t (LitString s)

        TermT t (Var v) ->
            k $ AExp $ ATerm t $ Var v

        TermT t (DCons d) ->
            k $ AExp $ ATerm t $ DCons d

        -- Probably the same way as IfThenElse !
        CaseT _ scrut ps ->
            normAtom scrut $ \scrut' -> do
                ps' <- mapM normPattern ps
                k $ CExp $ CCase scrut' ps'

-- both parts necessary?
normPattern (PatternT a b) =
    PExp <$> norm a <*> norm b

normAtom :: Show s => ExprT s
                   -> (AExp s -> State (AnfState s) (NExp s))
                   -> State (AnfState s) (NExp s)
normAtom e k =

    case e of

        -- Assumes v == lam == lam'
        LamT t vs body ->
            normExpr body $ \body' -> do
                v    <- symGen =<< get
                rest <- k $ ATerm t $ Var v
                pure $ NLet v
                            (AExp $ ALam t vs body')
                            rest

        -- assumes v == app == app'
        AppT t f xs ->
            normAtom f $ \f' ->
                normAtoms xs $ \xs' -> do
                    v    <- symGen =<< get
                    rest <- k $ ATerm t $ Var v
                    pure $ NLet v
                                (CExp $ CApp f' xs')
                                rest

        LetT _ a b c ->
            normExpr b $ \b' ->
                NLet a b' <$> normAtom c k

        -- assumes v == ite == ite'
        IfThenElseT t pr tr fl ->
            normAtom pr $ \pr' -> do
                v    <- symGen =<< get
                tr'  <- norm tr
                fl'  <- norm fl
                rest <- k $ ATerm t $ Var v
                pure $ NLet v
                            (CExp $ CIfThenElse pr' tr' fl')
                            rest

        UnPrimOpT _ op a ->
            normAtom a $ \a' ->
                k $ AUnPrimOp op a'

        BinPrimOpT _ op a b ->
            normAtom a $ \a' ->
                normAtom b $ \b' ->
                    k $ ABinPrimOp op a' b'

        TermT t (LitBool i) ->
            k $ ATerm t (LitBool i)

        TermT t (LitInt i) ->
            k $ ATerm t (LitInt i)

        TermT t (LitString s) ->
            k $ ATerm t (LitString s)

        TermT t (Var v) ->
            k $ ATerm t (Var v)

        TermT t (DCons d) ->
            k $ ATerm t (DCons d)

normAtoms :: Show s => [ExprT s]
                    -> ([AExp s] -> State (AnfState s) (NExp s))
                    -> State (AnfState s) (NExp s)
normAtoms [] k = k []
normAtoms (e:es) k =
    normAtom e $ \e' ->
        normAtoms es $ \es' ->
            k (e':es')
