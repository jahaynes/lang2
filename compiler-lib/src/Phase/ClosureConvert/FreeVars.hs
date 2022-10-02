module Phase.ClosureConvert.FreeVars (Scope (..), getFreeVars, withScope) where

import Common.State
import Core.Term
import Phase.Anf.AnfExpression

import           Data.Functor  ((<&>))
import           Data.Set      (Set)
import qualified Data.Set as S

newtype Scope s =
    Scope { getScope :: Set s }

getFreeVars :: (Ord s, Show s) => Set s -> NExp s -> Set s
getFreeVars scope n = evalState (nexpFreeVars n)
                    $ Scope scope

nexpFreeVars :: (Show s, Ord s) => NExp s -> State (Scope s) (Set s)
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

aexpFreeVars :: (Show s, Ord s) => AExp s -> State (Scope s) (Set s)
aexpFreeVars aexp =

    case aexp of

        ATerm t ->
            termFreeVars t

        ALam vs b ->
            withScope vs $
                nexpFreeVars b

        AUnPrimOp _ a ->
            aexpFreeVars a

        ABinPrimOp _ a b ->
            mconcat <$> mapM aexpFreeVars [a, b]

        AClo{} ->
            error "TODO: aexpFreeVars AClo"

cexpFreeVars :: (Show s, Ord s) => CExp s -> State (Scope s) (Set s)
cexpFreeVars cexp =

    case cexp of

        CApp f xs ->
            mconcat <$> mapM aexpFreeVars (f:xs)

        CIfThenElse pr tr fl -> do
            a <- aexpFreeVars pr
            b <- nexpFreeVars tr
            c <- nexpFreeVars fl
            pure $ mconcat [a, b, c]

        CCase scrut ps -> do
            a  <- aexpFreeVars scrut
            bs <- mapM pexpFreeVars ps
            pure $ mconcat (a:bs)

pexpFreeVars :: (Show s, Ord s) => PExp s -> State (Scope s) (Set s)
pexpFreeVars (PExp a b) =
    withScope (scopeFromPattern a) $
        nexpFreeVars b
    where
    scopeFromPattern lhs =
        case lhs of
            AExp aexp -> scopeFromAexp aexp
            CExp cexp -> scopeFromCexp cexp
        where
        scopeFromAexp aexp =
            case aexp of
                ATerm (LitInt {}) -> mempty
                ATerm (Var v)     -> [v]
                ATerm (DCons {})  -> mempty

        scopeFromCexp cexp =
            case cexp of
                CApp (ATerm DCons{}) args -> concatMap scopeFromAexp args

termFreeVars :: Ord s => Term s -> State (Scope s) (Set s)
termFreeVars t =
    case t of
        Var s       -> variableIfNotScoped s
        LitInt{}    -> pure mempty
        LitBool{}   -> pure mempty
        LitString{} -> pure mempty
        DCons{}     -> pure mempty

variableIfNotScoped :: Ord s => s -> State (Scope s) (Set s)
variableIfNotScoped v =
    (getScope <$> get) <&> \scope ->
        if S.member v scope
            then mempty
            else S.singleton v

withScope :: Ord s => [s]
                   -> State (Scope s) a
                   -> State (Scope s) a
withScope vs f = do
    initScope <- getScope <$> get
    modify' $ \fv -> fv { getScope = initScope <> S.fromList vs }
    x <- f
    modify' $ \fv -> fv { getScope = initScope }
    pure x
