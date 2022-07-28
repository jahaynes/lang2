{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.TypeInference (InferState (..), PartialInference (..), PolytypeEnv (..), infer, inferModule) where

import Common.State
import Core.Expression
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import TypeCheck.ConstraintSolver
import TypeCheck.TypeCheckTypes

import           Control.Monad         (foldM, forM, replicateM)
import           Data.Functor          ((<&>))
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S

data InferState s =
    InferState { getCount       :: !Int
               , getConstraints :: ![Constraint s]
               , getPolytypeEnv :: !(PolytypeEnv s)
               } deriving Show

newtype PolytypeEnv s =
    PolytypeEnv (Map s (Polytype s))
       deriving Show

-- TODO make typecheck plan from in here

inferModule :: Module ByteString
            -> TypeCheckPlan (Set ByteString)
            -> Either ByteString (TypedModule ByteString)
inferModule md (TypeCheckPlan tcp) = do

    let funDefnMap = M.fromList . map (\(FunDefn n e) -> (n, e)) $ getFunDefns md

        polyTypeEnv = PolytypeEnv (foldr (\(TypeSig n t) -> M.insert n (closeOver t)) M.empty $ getTypeSigs md)

        typeCheckPlan' = TypeCheckPlan $ map (map (\n -> (n, funDefnMap ! n)) . S.toList) tcp

    (typedExpressions, _polytypeEnv') <- foldM inferTopLevelGroup ([], polyTypeEnv) typeCheckPlan'

    pure $ TypedModule { getTFunDefns = map asTypedFunDefn typedExpressions }

asTypedFunDefn :: (s, AExpr (Polytype s) s) -> TFunDefn s
asTypedFunDefn (name, ex) = TFunDefn name ex

data PartialInference s =
    PartialInference { pi_typedExpr   :: !(AExpr (Type s) s)
                     , pi_constraints :: ![Constraint s]
                     } deriving Show

inferTopLevelGroup :: ([(ByteString, AExpr (Polytype ByteString) ByteString)], PolytypeEnv ByteString)
                   -> [(ByteString, Expr ByteString)]
                   -> Either ByteString ([(ByteString, AExpr (Polytype ByteString) ByteString)], PolytypeEnv ByteString)
inferTopLevelGroup (as, env) group =

    let (partialInferences, st) =

            runState' (InferState 0 [] env) $ do

                -- Make a fresh type variable for each top-level expression
                freshTopLevelTyVars <- mapM (\(n, e) -> fresh <&> \fv -> (n, fv, e)) group

                -- Make the fresh type variables available in scope
                modify' $ \is ->
                    let PolytypeEnv env' = getPolytypeEnv is
                    in is { getPolytypeEnv = PolytypeEnv $ foldr (\(n, fv, _) -> M.insert n (Forall [] fv)) env' freshTopLevelTyVars }

                -- Infer each definition's type (and constrain to the fresh variables)
                forM freshTopLevelTyVars $ \(n, fv, e) -> do

                    PartialInference te cs <- infer e
                    let ty = annot te

                    pure (n, PartialInference { pi_typedExpr   = te
                                              , pi_constraints = Constraint fv ty : cs })

    in updateEnv (as, getPolytypeEnv st) partialInferences

-- Constrain/closeover the inference results, and insert them into the environment
-- This is wrong
updateEnv :: ([(ByteString, AExpr (Polytype ByteString) ByteString)], PolytypeEnv ByteString)
          -> [(ByteString, PartialInference ByteString)]
          -> Either ByteString ([(ByteString, AExpr (Polytype ByteString) ByteString)], PolytypeEnv ByteString)
updateEnv (as, env) partialInferences = do
    subst <- runSolve (concatMap (pi_constraints . snd) partialInferences)

    pure $ foldr (go subst) (as, env) partialInferences
    where
    go :: Subst ByteString
       -> (ByteString, PartialInference ByteString)
       -> ([(ByteString, AExpr (Polytype ByteString) ByteString)], PolytypeEnv ByteString)
       -> ([(ByteString, AExpr (Polytype ByteString) ByteString)], PolytypeEnv ByteString)
    go subst (name, pinf) (tes, PolytypeEnv env') =
        --let typedExpr = mapAnnot (closeOver . substituteType subst)
        --              $ pi_typedExpr pinf
        let typedExpr = mapAnnot (generalize (PolytypeEnv mempty) . substituteType subst)
                      $ pi_typedExpr pinf
            -- avoiding normalize in this step
            -- note: normalize probably shouldn't be used on a functor!
            -- closeOver = normalize . generalize (PolytypeEnv mempty)
            polyType = annot typedExpr
        in ( (name, typedExpr):tes
           , PolytypeEnv (M.insert name polyType env') )

lookupPolytype :: ByteString -> State (InferState ByteString) (Type ByteString)
lookupPolytype x = do
    PolytypeEnv e <- getPolytypeEnv <$> get
    case M.lookup x e of
        Nothing -> error $ "unbound: " ++ show x
        Just p  -> instantiate p

infer :: Expr ByteString
      -> State (InferState ByteString) (PartialInference ByteString)
infer expr =

    case expr of

        ETerm b@LitBool{} ->
            pure PartialInference { pi_typedExpr   = ATerm typeBool b
                                  , pi_constraints = [] }

        ETerm i@LitInt{} ->
            pure PartialInference { pi_typedExpr   = ATerm typeInt i
                                  , pi_constraints = [] }

        ETerm s@LitString{} ->
            pure PartialInference { pi_typedExpr   = ATerm typeString s
                                  , pi_constraints = [] }

        ETerm v@(Var v') -> do
            t <- lookupPolytype v'
            pure PartialInference { pi_typedExpr   = ATerm t v
                                  , pi_constraints = [] }

        ETerm (DCons _) ->
            error "DCons not implemented"

        ELam vs e -> do
            tvs <- replicateM (length vs) fresh
            let scs = map (Forall []) tvs
            PartialInference e' cs <- inEnv (zip vs scs) (infer e)
            let ty = foldr TyArr (annot e') tvs
            let pi2 = PartialInference { pi_typedExpr   = ALam ty vs e'
                                       , pi_constraints = cs }
            pure pi2

        EApp f xs -> do
            PartialInference f' c1 <-      infer f
            inferences             <- mapM infer xs
            let xs' = map pi_typedExpr   inferences
            let ts  = map annot xs'
            let cs  = map pi_constraints inferences
            let t1  = annot f'
            tv       <- fresh
            pure PartialInference { pi_typedExpr   = AApp tv f' xs'
                                  , pi_constraints = c1 ++ concat cs ++ [Constraint t1 (foldr TyArr tv ts)] }

        ELet x e1 e2 -> do
            tv <- fresh
            PartialInference e1' c1 <- inEnv [(x, Forall [] tv)] $ infer e1
            let t1 = annot e1'
            case runSolve c1 of
                Left err -> error $ show err
                Right sub -> do
                    penv <- getPolytypeEnv <$> get
                    let sc = generalize (substitutePolytypeEnv sub penv) (substituteType sub t1)
                    PartialInference e2' c2 <- inEnv [(x, sc)] $ infer e2
                    let t2 = annot e2'
                    pure PartialInference { pi_typedExpr   = ALet t2 x e1' e2'
                                          , pi_constraints = c1 ++ c2 }

        EUnPrimOp op e -> do
            PartialInference e' c <- infer e
            tv <- fresh
            let t  = annot e'
                u1 = TyArr t tv
            u2 <- unOp op
            pure PartialInference { pi_typedExpr   = AUnPrimOp tv op e'
                                  , pi_constraints = c ++ [Constraint u1 u2] }

        EBinPrimOp op e1 e2 -> do
            PartialInference e1' c1 <- infer e1
            let t1 = annot e1'
            PartialInference e2' c2 <- infer e2
            let t2 = annot e2'
            tv <- fresh
            let u1 = t1 `TyArr` (t2 `TyArr` tv)
            u2 <- binOp op
            pure PartialInference { pi_typedExpr   = ABinPrimOp tv op e1' e2'
                                  , pi_constraints = c1 ++ c2 ++ [Constraint u1 u2] }

        IfThenElse p tr fl -> do
            PartialInference p' c1 <- infer p
            let t1 = annot p'
            PartialInference tr' c2 <- infer tr
            let t2 = annot tr'
            PartialInference fl' c3 <- infer fl
            let t3 = annot fl'
            pure PartialInference { pi_typedExpr   = AIfThenElse t2 p' tr' fl'
                                  , pi_constraints = c1 ++ c2 ++ c3 ++ [Constraint t1 typeBool, Constraint t2 t3] }

        EClo{} ->
            error "EClo doesn't exist yet"

        CallClo{} ->
            error "CallClo doesn't exist yet"

-- Run a state with a local environment, then revert environment
inEnv :: Ord s => [(s, Polytype s)] -> State (InferState s) a -> State (InferState s) a
inEnv xpts st = State $ \is ->
    let e@(PolytypeEnv env) = getPolytypeEnv is
        is' = is { getPolytypeEnv = PolytypeEnv $ foldr (\(x,pt) -> M.insert x pt) env xpts }
        (a, is'') = runState st is'
    in (a, is'' { getPolytypeEnv = e } )

unOp :: UnOp -> State (InferState ByteString) (Type ByteString)
unOp Negate = pure $ typeInt `TyArr` typeInt
unOp EShow  = fresh <&> \fr -> fr `TyArr` typeString
unOp _      = error "unop"

binOp :: BinOp -> State (InferState ByteString) (Type ByteString)
binOp AddI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp SubI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp MulI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp DivI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)
binOp ModI = pure $ typeInt `TyArr` (typeInt `TyArr` typeInt)

binOp EqA = fresh <&> \fr -> fr `TyArr` (fr `TyArr` typeBool)

binOp LtEqI = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)
binOp LtI   = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)
binOp GtEqI = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)
binOp GtI   = pure $ typeInt `TyArr` (typeInt `TyArr` typeBool)

binOp AndB = pure $ typeBool `TyArr` (typeBool `TyArr` typeBool)
binOp OrB  = pure $ typeBool `TyArr` (typeBool `TyArr` typeBool)

binOp ConcatS  = pure $ typeString `TyArr` (typeString `TyArr` typeString)

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
    put is { getCount = getCount is + 1 }
    pure $ TyVar (letters !! getCount is)

closeOver :: Type ByteString -> Polytype ByteString
closeOver = normalize . generalize (PolytypeEnv mempty)

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
