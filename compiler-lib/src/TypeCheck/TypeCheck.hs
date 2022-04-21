{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module TypeCheck.TypeCheck (runTypeCheck) where

import Core.Definition
import Core.Expression
import Core.Operator
import Core.Term
import Common.State
import TypeCheck.Types
import TypeCheck.TypedExpression

import           Control.Monad   (replicateM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor    ((<&>))
import           Data.Map        (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data TcState =
    TcState { getTypeEnv :: !TypeEnv
            , getCount   :: !Int
            } deriving Show

runTypeCheck :: [Defn ByteString] -> (TypeEnv, [TypedDefn Scheme ByteString])
runTypeCheck defns =

    let (typeSigs, funDefns) = foldl clas ([], []) defns
        typeEnv = buildTypeEnv typeSigs

        (typedExprs, tcState) = runState (mapM inferTop funDefns) $ TcState typeEnv 0

    in (getTypeEnv tcState, typedExprs)

    where
    clas (typeSigs, funDefns) t@TypeSig{} = (typeSigs++[t], funDefns)
    clas (typeSigs, funDefns) f@FunDefn{} = (typeSigs, funDefns++[f])
    clas (typeSigs, funDefns)           _ = (typeSigs, funDefns) -- TODO data types

    buildTypeEnv :: [Defn ByteString] -> TypeEnv
    buildTypeEnv ds = TypeEnv $ foldl bt mempty ds

        where
        bt :: Map ByteString Scheme -> Defn ByteString -> Map ByteString Scheme
        bt m (TypeSig n t) = M.insert n (closeOver t) m

inferTop :: Defn ByteString -> State TcState (TypedDefn Scheme ByteString)
inferTop (FunDefn name ex) = do
    (ty, cs, tye) <- infer ex
    let subst = runSolve cs
    let scheme = closeOver $ apply subst ty
    modify' $ \tcState ->
        tcState { getTypeEnv = extend (getTypeEnv tcState) (name, scheme)}
    pure $ FunDefnT scheme name (withTypes (closeOver . apply subst) tye)

infer :: Expr ByteString -> State TcState (Type ByteString, [Constraint], TypedExpr (Type ByteString) ByteString)
infer expr =
    case expr of
        ETerm b@LitBool{}   -> pure (TyCon "Bool", [], TermT (TyCon "Bool") b)
        ETerm i@LitInt{}    -> pure (TyCon "Int", [], TermT (TyCon "Int") i)
        ETerm s@LitString{} -> pure (TyCon "String", [], TermT (TyCon "String") s)
        ETerm v@(Var x)       -> do (t, c) <- lookupEnv x
                                    pure (t, c, TermT t v)
        ETerm (DCons _)     -> error "No typechecking for dataconstructors yet"

        ELam vs e -> do
            tvs <- replicateM (length vs) fresh
            let scs = map (Forall []) tvs
            (t, c, tye) <- inEnvs (zip vs scs) (infer e)
            let ty = foldr TyArr t tvs
            pure (ty, c, LamT ty vs tye)

        EApp f xs -> do
            (t1, c1, tyf) <- infer f
            (ts, cs, tyxs) <- unzip3 <$> mapM infer xs
            tv       <- fresh
            let c  = Constraint t1 (foldr TyArr tv ts)
                cs' = c1 ++ concat cs ++ [c]
            pure (tv, cs', AppT tv tyf tyxs)

        ELet x e1 e2 -> do
            env <- getTypeEnv <$> get
            (t1, c1, tye1) <- infer e1
            let sub = runSolve c1
                sc = generalize (apply sub env) (apply sub t1)
            (t2, c2, tye2) <- inEnv x sc $ local (apply sub) (infer e2)
            pure ( t2
                 , c1 ++ c2
                 , LetT t2 x tye1 tye2)

        EBinPrimOp op e1 e2 -> do
            (t1, c1, tye1) <- infer e1
            (t2, c2, tye2) <- infer e2
            tv <- fresh
            let u1 = t1 `TyArr` (t2 `TyArr` tv)
            u2 <- binOp op
            pure ( tv
                 , c1 ++ c2 ++ [Constraint u1 u2]
                 , BinPrimOpT tv op tye1 tye2 )

        EUnPrimOp op e -> do
            (t, c, tye) <- infer e
            tv <- fresh
            let u1 = TyArr t tv
            u2 <- unOp op
            pure (tv, c ++ [Constraint u1 u2], UnPrimOpT tv op tye)

        IfThenElse p t f -> do
            (t1, c1, typ) <- infer p
            (t2, c2, tyt) <- infer t
            (t3, c3, tyf) <- infer f
            pure ( t2
                 , c1 ++ c2 ++ c3 ++ [Constraint t1 (TyCon "Bool"), Constraint t2 t3]
                 , IfThenElseT t2 typ tyt tyf )

unOp :: UnOp -> State TcState (Type ByteString)
unOp Negate = pure $ TyCon "Int" `TyArr` TyCon "Int"
unOp EShow  = fresh <&> \fr -> fr `TyArr` TyCon "String"    -- TODO check
unOp Err    = fresh <&> \fr -> TyCon "String" `TyArr` fr    -- TODO check

binOp :: BinOp -> State TcState (Type ByteString)
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

inEnv :: ByteString -> Scheme -> State TcState a -> State TcState a
inEnv x sc m =
    let scope e = (remove e x) `extend` (x, sc)
    in local scope m

inEnvs :: [(ByteString, Scheme)] -> State TcState a -> State TcState a
inEnvs             [] m = m
inEnvs ((x, sc):xscs) m =
    let scope e = (remove e x) `extend` (x, sc)
    in local scope (inEnvs xscs m)

local :: (TypeEnv -> TypeEnv) -> State TcState a -> State TcState a
local f state = do
    oldEnv <- getTypeEnv <$> get
    let env' = f oldEnv
    modify' $ \tcState -> tcState { getTypeEnv = env' }
    r <- state
    modify' $ \tcState -> tcState { getTypeEnv = oldEnv }
    pure r

extend :: TypeEnv -> (ByteString, Scheme) -> TypeEnv
extend env (x, s) = env { types = M.insert x s (types env) }

remove :: TypeEnv -> ByteString -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

lookupEnv :: ByteString -> State TcState (Type ByteString, [Constraint])
lookupEnv x = do
  TypeEnv env <- getTypeEnv <$> get
  case M.lookup x env of

      Nothing ->
          error $ "Unbound: " ++ show x

      Just s -> do
          t <- instantiate s
          pure (t, [])

instantiate :: Scheme -> State TcState (Type ByteString)
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ M.fromList $ zip as as'
    pure $ apply s t

closeOver :: Type ByteString -> Scheme
closeOver = normalize . generalize emptyEnv

    where
    normalize :: Scheme -> Scheme
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
                Just x -> TyVar x
                Nothing -> error "type variable not in signature"

generalize :: TypeEnv -> (Type ByteString) -> Scheme
generalize env t = 
    let as = S.toList $ ftv t `S.difference` ftv env
    in Forall as t

fresh :: State TcState (Type ByteString)
fresh = do
    s <- get
    put s { getCount = getCount s + 1 }
    pure $ TyVar (letters !! getCount s)

letters :: [ByteString]
letters = C8.pack <$> ([1..] >>= flip replicateM ['a'..'z'])

type Unifier = (Subst, [Constraint])

runSolve :: [Constraint] -> Subst
runSolve cs = solver (Subst mempty, cs)

solver :: Unifier -> Subst
solver (su, cs) =
  case cs of
    [] -> su
    (Constraint t1 t2: cs0) -> do
      let su1 = unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ M.map (apply (Subst s1)) s2 `M.union` s1

unifies :: Type ByteString -> Type ByteString -> Subst
unifies t1 t2 | t1 == t2 = Subst mempty
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = error $ "UnificationFail: " ++ show (t1, t2)

bind ::  ByteString -> Type ByteString -> Subst
bind a t | t == TyVar a = Subst mempty
         | occursCheck  = error $ "InfiniteType: " ++ show (a, t)
         | otherwise    = Subst $ M.singleton a t

    where
    occursCheck :: Bool
    occursCheck = a `S.member` ftv t

unifyMany :: [Type ByteString] -> [Type ByteString] -> Subst
unifyMany [] [] = Subst mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  let su1 = unifies t1 t2
      su2 = unifyMany (apply su1 ts1) (apply su1 ts2)
  su2 `compose` su1
unifyMany t1 t2 = error $ "UnificationMismatch: " ++ show (t1, t2)