{-# LANGUAGE LambdaCase #-}

module Phase.ClosureConvert.FreeVars ( Scope (..)
                                     , getFreeVars
                                     , withScope ) where

import Common.EitherT          (EitherT (..), left)
import Common.State
import Common.Trans
import Core.Term
import Phase.Anf.AnfExpression

import           Data.ByteString.Char8       (ByteString, pack)
import           Data.Functor  ((<&>))
import           Data.Set      (Set)
import qualified Data.Set as S

newtype Scope s =
    Scope { getScope :: Set s }

type Fv s a =
    EitherT ByteString (
        State (Scope s)) a

-- TODO probably doesn't need Either, except for dev
getFreeVars :: (Ord s, Show s) => Set s -> NExp s -> Either ByteString (Set s)
getFreeVars scope n = evalState
                        (runEitherT (nexpFreeVars n))
                        (Scope scope)

nexpFreeVars :: (Show s, Ord s) => NExp s -> Fv s (Set s)
nexpFreeVars nexp =

    case nexp of

        AExp aexp ->
            aexpFreeVars aexp

        CExp cexp ->
            cexpFreeVars cexp

        NLet a b c ->
            withScope [a] $
                mappend <$> nexpFreeVars b
                        <*> nexpFreeVars c

aexpFreeVars :: (Show s, Ord s) => AExp s -> Fv s (Set s)
aexpFreeVars aexp =

    case aexp of

        ATerm _ term ->
            termFreeVars term

        ALam _ vs b ->
            withScope vs $
                nexpFreeVars b

        AUnPrimOp _ _ a ->
            aexpFreeVars a

        ABinPrimOp _ _ a b ->
            mconcat <$> mapM aexpFreeVars [a, b]

        AClo{} ->
            error "TODO: aexpFreeVars AClo"

cexpFreeVars :: (Show s, Ord s) => CExp s -> Fv s (Set s)
cexpFreeVars cexp =

    case cexp of

        CApp _ f xs ->
            mconcat <$> mapM aexpFreeVars (f:xs)

        CIfThenElse _ pr tr fl -> do
            a <- aexpFreeVars pr
            b <- nexpFreeVars tr
            c <- nexpFreeVars fl
            pure $ mconcat [a, b, c]

        CCase _ scrut ps -> do
            a  <- aexpFreeVars scrut
            bs <- mapM pexpFreeVars ps
            pure $ mconcat (a:bs)

pexpFreeVars :: (Show s, Ord s) => PExp s -> Fv s (Set s)
pexpFreeVars (PExp a b) =
    withScope (scopeFromPattern a)
              (nexpFreeVars b)

    where
    scopeFromPattern (PApp _ _dc terms) = concatMap scopeFromTerm terms

    scopeFromTerm = \case
        LitBool{} -> mempty
        LitInt{}  -> mempty
        (Var v)   -> [v]

termFreeVars :: Ord s => Term s -> Fv s (Set s)
termFreeVars t =
    case t of
        Var s       -> variableIfNotScoped s
        LitInt{}    -> pure mempty
        LitBool{}   -> pure mempty
        LitString{} -> pure mempty
        DCons{}     -> pure mempty

variableIfNotScoped :: Ord s => s -> Fv s (Set s)
variableIfNotScoped v = do
    scope <- getScope <$> lift get
    pure $ if S.member v scope
               then mempty
               else S.singleton v

withScope :: Ord s => [s]
                   -> Fv s a
                   -> Fv s a
withScope vs f = do
    initScope <- getScope <$> lift get
    lift . modify' $ \fv -> fv { getScope = initScope <> S.fromList vs }
    x <- f
    lift . modify' $ \fv -> fv { getScope = initScope }
    pure x
