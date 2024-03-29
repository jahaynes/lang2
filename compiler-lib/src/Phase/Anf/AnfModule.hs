{-# LANGUAGE OverloadedStrings #-}

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

import Data.ByteString.Char8 (ByteString, pack)

data AnfModule s =
    AnfModule { getDataDefnAnfTs :: [DataDefn s]
              , getFunDefAnfTs   :: [FunDefAnfT s]
              } deriving Show

type Anf s a =
    EitherT ByteString (
        State (AnfState s)) a

anfModule :: Module (Type ByteString) ByteString
          -> Either ByteString (AnfModule ByteString)
anfModule md = AnfModule (getDataDefns md) <$> mapM anfFunDefT (getFunDefns md)

data FunDefAnfT s =
    FunDefAnfT s (Quant s) (NExp s)
        deriving Show

anfFunDefT :: FunDefn (Type ByteString) ByteString
           -> Either ByteString (FunDefAnfT ByteString)
anfFunDefT (FunDefn n pt expr) = FunDefAnfT n pt <$> evalState (runEitherT (norm expr)) (AnfState 0 genSym)

data AnfState s =
    AnfState { getNum :: Int
             , symGen :: State (AnfState s) s
             }

genSym :: State (AnfState s) ByteString
genSym = do
    AnfState n sg <- get
    put $! AnfState (n+1) sg
    pure . pack $ "anf_" <> show n

norm :: Show s => Expr (Type s) s -> Anf s (NExp s)
norm expr = normExpr expr pure

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
