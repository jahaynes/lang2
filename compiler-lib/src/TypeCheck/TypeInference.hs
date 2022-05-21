{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.TypeInference (InferState (..), PolytypeEnv (..), infer, inferModule) where

import Common.State
import Core.Expression
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import TypeCheck.ConstraintSolver
import TypeCheck.TypeCheckTypes

import           Control.Monad         (forM, replicateM)
import           Data.Functor          ((<&>))
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S

data InferState s =
    InferState { count       :: !Int
               , constraints :: ![Constraint s]
               , polytypeEnv :: !(PolytypeEnv s)
               } deriving Show

newtype PolytypeEnv s =
    PolytypeEnv (Map s (Polytype s))
       deriving Show

inferModule :: Module ByteString -> [Set ByteString] -> Either ByteString (PolytypeEnv ByteString)
inferModule md = do

    let funDefnMap = M.fromList . map (\(FunDefn n e) -> (n, e)) $ getFunDefns md

        polyTypeEnv = PolytypeEnv (foldr (\(TypeSig n t) -> M.insert n (closeOver t)) M.empty $ getTypeSigs md)

    go funDefnMap polyTypeEnv

    where
    go       _ env [] = Right env
    go defnMap env (p:ps) = do

        let ns = S.toList p

        let (tycs, st'') =

                runState' (InferState 0 [] env) $ do

                    -- Make fresh type variables for each top-level
                    nfvs <- mapM (\n -> fresh <&> \f -> (n, f)) ns

                    modify' $ \is ->
                        let PolytypeEnv env' = polytypeEnv is
                        in is { polytypeEnv = PolytypeEnv $ foldr (\(n, t) -> M.insert n (Forall [] t)) env' nfvs }

                    -- Infer each definition's type (and constrain to the fresh variables)
                    forM nfvs $ \(n, fv) -> do
                        (ty, cs) <- infer (defnMap ! n)
                        let c = Constraint fv ty
                        pure (n, (ty, c : cs))

        let cs = concatMap (snd . snd) tycs

        subst <- runSolve cs

        let tys = map (\(n, (t, _)) -> (n, closeOver $ substituteType subst t)) tycs
            env' = PolytypeEnv (let PolytypeEnv e = polytypeEnv st'' in foldr (\(n,pt) -> M.insert n pt) e tys)
        go defnMap env' ps

lookupPolytype :: ByteString -> State (InferState ByteString) (Type ByteString)
lookupPolytype x = do
    PolytypeEnv e <- polytypeEnv <$> get
    case M.lookup x e of
        Nothing -> error $ "unbound: " ++ show x
        Just p  -> instantiate p

infer :: Expr ByteString
      -> State (InferState ByteString) (Type ByteString, [Constraint ByteString])
infer expr =

    case expr of

        ETerm (LitBool _) ->
            pure (TyCon "Bool", [])

        ETerm (LitInt _) ->
            pure (TyCon "Int", [])

        ETerm (LitString _) ->
            pure (TyCon "String", [])

        ETerm (Var v) -> do
            t <- lookupPolytype v
            pure (t, [])

        ETerm (DCons _) ->
            error "DCons not implemented"

        ELam vs e -> do
            tvs <- replicateM (length vs) fresh
            let scs = map (Forall []) tvs
            (t, cs) <- inEnv (zip vs scs) (infer e)
            let ty = foldr TyArr t tvs
            pure (ty, cs)

        EApp f xs -> do
            (t1, c1) <- infer f
            (ts, cs) <- unzip <$> mapM infer xs
            tv       <- fresh
            pure (tv, c1 ++ concat cs ++ [Constraint t1 (foldr TyArr tv ts)])

        ELet x e1 e2 -> do

            tv <- fresh
            (t1, c1) <- inEnv [(x, Forall [] tv)] $ infer e1

            case runSolve c1 of
                Left err -> error $ show err
                Right sub -> do
                    penv <- polytypeEnv <$> get
                    let sc = generalize (substitutePolytypeEnv sub penv) (substituteType sub t1)
                    (t2, c2) <- inEnv [(x, sc)] $ infer e2
                    pure (t2, c1 ++ c2)

        EUnPrimOp op e -> do
            (t, c) <- infer e
            tv <- fresh
            let u1 = TyArr t tv
                u2 = unOp op
            pure (tv, c ++ [Constraint u1 u2])

        EBinPrimOp op e1 e2 -> do
            (t1, c1) <- infer e1
            (t2, c2) <- infer e2
            tv <- fresh
            let u1 = t1 `TyArr` (t2 `TyArr` tv)
            u2 <- binOp op
            pure (tv, c1 ++ c2 ++ [Constraint u1 u2])

        IfThenElse p tr fl -> do
            (t1, c1) <- infer p
            (t2, c2) <- infer tr
            (t3, c3) <- infer fl
            pure (t2, c1 ++ c2 ++ c3 ++ [Constraint t1 (TyCon "Bool"), Constraint t2 t3])

        EClo{} ->
            error "EClo doesn't exist yet"

        CallClo{} ->
            error "CallClo doesn't exist yet"

-- Run a state with a local environment, then revert environment
inEnv :: Ord s => [(s, Polytype s)] -> State (InferState s) a -> State (InferState s) a
inEnv xpts st = State $ \is ->
    let e@(PolytypeEnv env) = polytypeEnv is
        is' = is { polytypeEnv = PolytypeEnv $ foldr (\(x,pt) -> M.insert x pt) env xpts }
        (a, is'') = runState st is'
    in (a, is'' { polytypeEnv = e } )

unOp :: UnOp -> Type ByteString
unOp Negate = TyCon "Int" `TyArr` TyCon "Int"
unOp _      = error "unop"

binOp :: BinOp -> State (InferState ByteString) (Type ByteString)
binOp AddI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Int")
binOp SubI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Int")
binOp MulI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Int")
binOp DivI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Int")
binOp ModI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Int")

binOp EqA = fresh <&> \fr -> fr `TyArr` (fr `TyArr` TyCon "Bool")

binOp LtEqI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Bool")
binOp LtI   = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Bool")
binOp GtEqI = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Bool")
binOp GtI   = pure $ TyCon "Int" `TyArr` (TyCon "Int" `TyArr` TyCon "Bool")

binOp AndB = pure $ TyCon "Bool" `TyArr` (TyCon "Bool" `TyArr` TyCon "Bool")
binOp OrB  = pure $ TyCon "Bool" `TyArr` (TyCon "Bool" `TyArr` TyCon "Bool")

binOp ConcatS  = pure $ TyCon "String" `TyArr` (TyCon "String" `TyArr` TyCon "String")

substitutePolytypeEnv :: Ord s => Subst s -> PolytypeEnv s -> PolytypeEnv s
substitutePolytypeEnv s (PolytypeEnv e) = PolytypeEnv $ M.map (substitutePolytype s) e

substitutePolytype :: Ord s => Subst s -> Polytype s -> Polytype s
substitutePolytype (Subst s) (Forall as t) =
    let s' = Subst $ foldr M.delete s as
    in Forall as $ substituteType s' t

instantiate ::  Polytype ByteString -> State (InferState ByteString) (Type ByteString)
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ M.fromList $ zip as as'
    pure $ substituteType s t

letters :: [ByteString]
letters = map pack ([1..] >>= flip replicateM ['a'..'z'])

fresh :: State (InferState ByteString) (Type ByteString)
fresh = do
    is <- get
    put is { count = count is + 1 }
    pure $ TyVar (letters !! count is)

closeOver :: Type ByteString -> Polytype ByteString
closeOver = normalize . generalize (PolytypeEnv mempty)

    where
    normalize :: Polytype ByteString -> Polytype ByteString
    normalize (Forall _ body) = Forall (map snd ord) (normtype body)
        where
        ord = zip (nub $ fv body) letters

        nub = S.toList . S.fromList

        fv (TyVar a)   = [a]
        fv (TyArr a b) = fv a ++ fv b
        fv (TyCon _)   = []

        normtype (TyArr a b) = TyArr (normtype a) (normtype b)
        normtype (TyCon a)   = TyCon a
        normtype (TyVar a)   =
            case lookup a ord of
                Just x  -> TyVar x
                Nothing -> error "type variable not in signature"

generalize :: Ord s => PolytypeEnv s -> Type s -> Polytype s
generalize (PolytypeEnv env) t = Forall as t
    where
    as = S.toList $ freeInType t `S.difference` freeInEnv

    freeInEnv = freeInList $ M.elems env

        where
        freeInList = foldr (S.union . freeInPolytype) S.empty

        freeInPolytype (Forall pas pt) = freeInType pt `S.difference` S.fromList pas
