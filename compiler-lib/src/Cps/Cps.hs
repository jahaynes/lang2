module Cps.Cps (cps) where

import Common.State
import Core.Definition
import Core.Expression
import Core.Term
import Data.ByteString.Char8 (ByteString, pack)

data CpsState s =
    CpsState { _count  :: !Int
             , _symGen :: !(Int -> s)
             }

cps :: Module ByteString -> Module ByteString
cps md =
    let funDefs' = evalState (mapM cpsFunDef $ getFunDefns md) (CpsState 0 (\n -> pack $ "c" <> show n))
    in md { getFunDefns = funDefs' }

cpsFunDef :: FunDefn s -> State (CpsState s) (FunDefn s)
cpsFunDef (FunDefn s expr) = do
    -- Force top-levels as lambdas so they can be called with a continuation
    let expr' =
          case expr of
              ELam{} -> expr
              _      -> ELam [] expr
    FunDefn s <$> cpsM expr'

cpsM :: Expr s
     -> State (CpsState s) (Expr s)

cpsM e@ETerm{} = pure e

cpsM (ELam vs body) = do
    k     <- genSym
    body' <- cpsC body k
    pure $ ELam (vs ++ [k]) body'

cpsM e = do
    k     <- genSym
    body' <- cpsC e k
    pure $ ELam [k] body'

cpsC :: Expr s
     -> s
     -> State (CpsState s) (Expr s)
cpsC e@ELam{} c = do
    e' <- cpsM e
    pure $ EApp (ETerm $ Var c) [e']

cpsC e@ETerm{} c = do
    e' <- cpsM e
    pure $ EApp (ETerm $ Var c) [e']

cpsC (ELet x y z) c =
    cpsK y $ \y' -> do
        z' <- cpsC z c
        pure $ ELet x y' z'

cpsC (EApp f es) c =
    cpsK f $ \f' ->
        cpsKs es $ \es' ->
            pure $ EApp f' (es' ++ [ETerm $ Var c])

cpsC (EUnPrimOp o a) c =
    cpsK a $ \a' -> do
        v <- genSym
        c' <- cpsC (ETerm $ Var v) c
        pure $ ELet v (EUnPrimOp o a') c'

cpsC (EBinPrimOp o a b) c =
    cpsK a $ \a' ->
        cpsK b $ \b' -> do
            v  <- genSym
            c' <- cpsC (ETerm $ Var v) c
            pure $ ELet v (EBinPrimOp o a' b') c'

cpsC (IfThenElse p t f) c = do
    k <- genSym
    body <- cpsK p $ \p' -> do
                t' <- cpsC t k
                f' <- cpsC f k
                pure $ IfThenElse p' t' f'
    pure $ ELet k (ETerm (Var c)) body

cpsC EClo{} _ =
    error "Does not exist yet!"

cpsC CallClo{} _ =
    error "Does not exist yet!"

genSym :: State (CpsState s) s
genSym = do
    CpsState n f <- get
    put $! CpsState (n + 1) f
    pure $ f n

cpsK :: Expr s
     -> (Expr s -> State (CpsState s) (Expr s))
     -> State (CpsState s) (Expr s)
cpsK expr@ELam{}  k = k =<< cpsM expr
cpsK expr@ETerm{} k = k =<< cpsM expr

cpsK (EBinPrimOp o a b) k =
    cpsK a $ \a' ->
        cpsK b $ \b' -> do
            r <- genSym
            cont <- k (ETerm (Var r))
            pure $ ELet r (EBinPrimOp o a' b') cont

cpsK (EApp f xs) k = do
    r <- genSym
    kr <- k (ETerm (Var r))
    cpsK f $ \f' ->
        cpsKs xs $ \xs' ->
            pure $ EApp f' (xs' ++ [ELam [r] kr])

cpsK (ELet x y z) k =
    cpsK y $ \y' -> do
        z' <- cpsK z k
        pure $ ELet x y' z'

cpsK (EUnPrimOp o e) k =
    cpsK e $ \e' -> do
            r <- genSym
            cont <- k (ETerm (Var r))
            pure $ ELet r (EUnPrimOp o e') cont

cpsK (IfThenElse p t f) k = do
    c <- genSym
    t' <- cpsC t c
    f' <- cpsC f c
    v <- genSym
    k' <- k (ETerm (Var v))
    cpsK p $ \p' ->
        pure $ ELet c (ELam [v] k') (IfThenElse p' t' f')

cpsK EClo{} _ =
    error "Does not exist yet!"

cpsK CallClo{} _ =
    error "Does not exist yet!"

cpsKs :: [Expr s]
      -> ([Expr s] -> State (CpsState s) (Expr s))
      -> State (CpsState s) (Expr s)
cpsKs     [] k = k []
cpsKs (e:es) k =
    cpsK e $ \e' ->
        cpsKs es $ \es' ->
            k (e':es')
