{-# LANGUAGE OverloadedStrings #-}

module Phase.Anf.AnfTransform where

import Common.EitherT
import Common.State
import Common.Trans
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Phase.Anf.Anf

import Data.ByteString.Char8 (ByteString, pack)

type Anf s a =
    EitherT ByteString (
        State (AnfState s)) a

{-

    main = let x = 3 in x + x + x


main : some type
main =
  let anf_0 = x + x in
  let x = 3 in
  anf_0 + x


-}


-- May be able to make this infallible
anfModule :: Module (Type ByteString) ByteString
          -> Either ByteString (AnfModule ByteString)
anfModule md = AnfModule (getDataDefns md) <$> mapM anfFunDefT (getFunDefns md)

anfFunDefT :: FunDefn (Type ByteString) ByteString
           -> Either ByteString (FunDefAnfT ByteString)
anfFunDefT (FunDefn n pt expr) = FunDefAnfT n pt <$> evalState (runEitherT (norm expr)) (AnfState 0 (genSym "anf_") (genSym "ll_"))

data AnfState s =
    AnfState { getNum :: Int
             , genAnf :: State (AnfState s) s
             , genLam :: State (AnfState s) s
             }

genSym :: ByteString -> State (AnfState s) ByteString
genSym pre = do
    AnfState n sg lg <- get
    put $! AnfState (n+1) sg lg
    pure (pre <> (pack $ show n))

norm :: Show s => Expr (Type s) s -> Anf s (NExp s)
norm expr = asAnfExpr expr pure

asAnfExpr :: Show s => Expr (Type s) s
                    -> (NExp s -> Anf s (NExp s))
                    -> Anf s (NExp s)
asAnfExpr expr k =

    case expr of

        Term t term ->
            k (AExp $ ATerm t term)

        Lam t _vs _body -> do
            -- left "TODO: lift out lambda/closure"
            ll <- lift (genLam =<< get) -- pretend lifting
            k (AExp $  ATerm t (Var ll))

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
            asAtomicExpr pr $ \pr' ->
                asAnfExpr tr $ \tr' ->
                    asAnfExpr fl $ \fl' ->
                        k (CExp $ CIfThenElse t pr' tr' fl')

        Case _t _scr _ps ->
            undefined

asAtomicExpr :: Show s => Expr (Type s) s
                       -> (AExp s -> Anf s (NExp s))
                       -> Anf s (NExp s)
asAtomicExpr expr k =

    case expr of

        Term t term ->
            k (ATerm t term)

        Lam t _vs _body -> do
            -- left "TODO: lift out lambda/closure"
            ll <- lift (genLam =<< get) -- pretend lifting
            k (ATerm t (Var ll))

        App t f xs ->
            asAtomicExpr f $ \f' ->
                asAtomicExprs xs $ \xs' -> do
                    s <- lift (genAnf =<< get)
                    NLet t s (CExp $ CApp t f' xs') <$> k (ATerm t (Var s))

        Let t a b c ->

            asAnfExpr b $ \b' ->
                NLet t a b' <$> asAtomicExpr c k

        UnPrimOp _t _op _a ->
            left "TODO un"

        BinPrimOp t op a b ->
            asAtomicExpr a $ \a' ->
                asAtomicExpr b $ \b' -> do
                    s    <- lift (genAnf =<< get)
                    rest <- k (ATerm t (Var s))
                    pure $ NLet t s (CExp $ CBinPrimOp t op a' b') rest

        IfThenElse t pr tr fl ->
            asAtomicExpr pr $ \pr' -> do
                v    <- lift (genAnf =<< get)
                tr'  <- norm tr
                fl'  <- norm fl
                rest <- k $ ATerm t $ Var v
                
                -- This type is wrong!
                pure $ NLet t v (CExp $ CIfThenElse t pr' tr' fl') rest

        Case _t _scr _ps ->
            left "TODO case"

asAtomicExprs :: Show s => [Expr (Type s) s]
                        -> ([AExp s] -> Anf s (NExp s))
                        -> Anf s (NExp s)
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
