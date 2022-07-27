{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Cps.CpsTyped (cps) where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Data.ByteString.Char8 (ByteString, pack)
import Debug.Trace  (trace)

data CpsState s =
    CpsState { _count  :: !Int
             , _symGen :: !(Int -> s)
             }

cps :: TypedModule ByteString -> TypedModule ByteString
cps md =
    let funDefs' = evalState (mapM cpsFunDef $ getTFunDefns md) (CpsState 0 (\n -> pack $ "c" <> show n))
    in md { getTFunDefns = funDefs' }

cpsFunDef :: TFunDefn ByteString
          -> State (CpsState ByteString) (TFunDefn ByteString)
cpsFunDef (TFunDefn s expr) = TFunDefn s <$> cpsM expr

haltType :: Type ByteString
haltType = TyCon "z"

polyArity :: Polytype s -> Int
polyArity (Forall _ t) = arity t

arity :: Type s -> Int
arity = go 0
    where
    go acc (TyArr _ b) = go (1+acc) b
    go acc           _ = acc

getArgTypes' :: Type s -> [Type s]
getArgTypes' = go []
    where
    go acc (TyArr a b) = go (a:acc) b
    go acc           _ = reverse acc

getReturnType' :: Type s -> Type s
getReturnType' = go
    where
    go (TyArr _ b) = go b
    go           a = a

{-
    TOTHINK: maybe simple top-level constants should also be contified?
-}

cpsMLamT :: Polytype ByteString -> Polytype ByteString
cpsMLamT (Forall _ lt) =
    let argTypes = getArgTypes' lt
        retType  = getReturnType' lt
        ct       = retType `TyArr` haltType
    in Forall [] . rebuild $ argTypes ++ [ct, haltType]

rebuild    [t] = t
rebuild (t:ts) = t `TyArr` rebuild ts
rebuild     [] = error "No type!"

cpsM :: AExpr (Polytype ByteString) ByteString
     -> State (CpsState ByteString) (AExpr (Polytype ByteString) ByteString)

cpsM e@(ATerm t term)

    -- This should adjust the type of 'foo' at call sites to 'foo'
    | polyArity t > 0 = pure (ATerm (cpsMLamT t) term)

    -- Should this also be adjusted?
    | otherwise = pure e

cpsM (ALam t vs body) = do
    c     <- genSym
    -- hypothesis. ct must turn body into haltType -- TODO probably wrong!
    let ct = let Forall _ bt = annot body in Forall [] (bt `TyArr` haltType)
    body' <- cpsC body (c, ct)
    pure $ ALam (cpsMLamT t) (vs ++ [c]) body'

cpsM e =
    pure e

tyArr' :: Polytype s -> Polytype s -> Polytype s
tyArr' (Forall _ a) (Forall _ b) = Forall [] (a `TyArr` b)

ut :: Int -> Polytype ByteString
ut n = Forall [] . TyCon . pack $ "u" <> show n

-- polyTypeApply :: (Eq s, Show s) => [Char] -> Polytype s -> [Polytype s] -> Polytype s
polyTypeApply msg pf pxs =
    let f'  = mono pf
        xs' = map mono pxs
    in Forall [] $ typeApply msg f' xs'

    where
    -- typeApply :: (Eq s, Show s) => [Char] -> Type s -> [Type s] -> Type s
    typeApply   _           f     [] = f
    typeApply msg (TyArr a b) (x:xs)
        | a == x = typeApply msg b xs
        | otherwise =
            case a of
                -- try again down left side? a is arrow?
                TyArr a' b' -> do
                    let foo = TyArr a' (TyArr b' b) --- eeehh???
                    typeApply msg foo (x:xs)
                _ -> mono $ ut 8
    typeApply _ _ _ = mono $ ut 9

mono :: Polytype s -> Type s
mono (Forall _ t) = t

-- bad q
resolve :: Polytype ByteString -> [a] -> [Type ByteString]
resolve = go []
    where
    go acc                      t     [] = reverse acc
    go acc (Forall q (TyArr a b)) (_:vs) = go (a:acc) (Forall q b) vs

cpsC :: AExpr (Polytype ByteString) ByteString
     -> (ByteString, Polytype ByteString)
     -> State (CpsState ByteString) (AExpr (Polytype ByteString) ByteString)

cpsC e@ATerm{} (c, ct) = do
    e' <- cpsM e
    -- Hypothesis: type apply is fine here
    let appt = polyTypeApply ("cpsC term: " ++ show (ct, [annot e'])) ct [annot e']
    pure $ AApp appt
                (ATerm ct $ Var c)
                [e']

cpsC e@(ALam t vs body) (c, ct) = do

    e' <- cpsM e
    let et' = annot e'

    -- guess
    let appt = polyTypeApply "cpsC Lam" ct [et']

    pure $ AApp appt (ATerm ct (Var c)) [e']

cpsC e@(AApp t f es) k@(c, ct) = -- bad ct here?
    cpsK f $ \f' ->
        cpsKs es $ \es' -> do
                -- Hypothesis: ct is type of c
            let args = es' ++ [ATerm ct $ Var c]
                -- Hypothesis: type apply is fine here
                appt = polyTypeApply ("cpsC app") (annot f') (map annot args)
            pure $ AApp appt f' args

cpsC (ALet _ x y z) (c, ct) =
    cpsK y $ \y' -> do
        -- Hypothesis: ct is passed down unchanged
        z' <- cpsC z (c, ct)
        -- Hypothesis: Let has the type of its last operand
        let lett = annot z'
        pure $ ALet lett x y' z'

cpsC (AUnPrimOp t o a) (c, ct) =
    cpsK a $ \a' -> do
        v  <- genSym
        c' <- cpsC (ATerm t $ Var v) (c, ct)
        -- Hypothesis: Let has the type of its last operand
        let lett = annot c'
        pure $ ALet lett v (AUnPrimOp t o a') c'

cpsC (ABinPrimOp t o a b) (c, ct) =
    cpsK a $ \a' ->
        cpsK b $ \b' -> do
            v  <- genSym
            c' <- cpsC (ATerm t $ Var v) (c, ct)
            -- Hypothesis: Let has the type of its last operand
            let lett = annot c'
            pure $ ALet lett v (ABinPrimOp t o a' b') c'

-- Weird about the _
cpsC (AIfThenElse _ pr tr fl) (c, ct) = do
    k <- genSym
    -- Hypothesis: since c is assigned to k (check?), they must share a type
    let kt = ct
    body <- cpsK pr $ \pr' -> do
                tr' <- cpsC tr (k, kt)
                fl' <- cpsC fl (k, kt)
                -- Hypothesis: the two branches must match type
                -- and that becomes the IF's overall type
                let ift = both (annot tr') (annot fl')
                pure $ AIfThenElse ift pr' tr' fl'
    -- Hypothesis: Let has the type of its last operand
    let lett = annot body
    --Hypothesis: ct is type of c
    pure $ ALet lett k (ATerm ct (Var c)) body

cpsC AClo{} _ =
    error "Too early for closures"

cpsC ACallClo{} _ =
    error "Too early for closures"

both :: (Eq a, Show a) => a -> a -> a
both a b
    | a == b    = a
    | otherwise = error $ "expected equal: " ++ show (a, b)

cpsK :: AExpr (Polytype ByteString) ByteString
     -> (AExpr (Polytype ByteString) ByteString -> State (CpsState ByteString) (AExpr (Polytype ByteString) ByteString))
     -> State (CpsState ByteString) (AExpr (Polytype ByteString) ByteString)

cpsK expr@ATerm{} k =
    k =<< cpsM expr

cpsK expr@ALam{} k =
    k =<< cpsM expr

cpsK (AApp _ f xs) k = do
    r  <- genSym
    -- Guess
    rt <- TyVar <$> genSym
    kr <- k (ATerm (Forall [] rt) (Var r))
    cpsK f $ \f' ->
        cpsKs xs $ \xs' -> do

            let krt = mono $ annot kr
            let tlam = Forall [] (rt `TyArr` krt)

            let args = xs' ++ [ALam tlam [r] kr]

            let appt = polyTypeApply "cpsK App" (annot f') (map annot args)

            pure $ AApp appt f' args

cpsK (ALet _ _ _ _) _ =
    undefined

cpsK (AUnPrimOp t o e) k =
    cpsK e $ \e' -> do
            r <- genSym
            cont <- k (ATerm t (Var r))
            -- Hypothesis: Let has the type of its last operand
            let lett = annot cont
            pure $ ALet lett r (AUnPrimOp t o e') cont

cpsK (ABinPrimOp t o a b) k =
    cpsK a $ \a' ->
        cpsK b $ \b' -> do
            r <- genSym
            cont <- k (ATerm t (Var r))
            -- Hypothesis: Let has the type of its last operand
            let lett = annot cont
            pure $ ALet lett r (ABinPrimOp t o a' b') cont

cpsK (AIfThenElse _ _ _ _) _ =
    undefined

cpsK AClo{} _ =
    error "Too early for closures"

cpsK ACallClo{} _ =
    error "Too early for closures"

cpsKs :: [AExpr (Polytype ByteString) ByteString]
      -> ([AExpr (Polytype ByteString) ByteString] -> State (CpsState ByteString) (AExpr (Polytype ByteString) ByteString))
      -> State (CpsState ByteString) (AExpr (Polytype ByteString) ByteString)
cpsKs     [] k = k []
cpsKs (e:es) k =
    cpsK e $ \e' ->
        cpsKs es $ \es' ->
            k (e':es')

genSym :: State (CpsState s) s
genSym = do
    CpsState n f <- get
    put $! CpsState (n + 1) f
    pure $ f n
