module TypeSystem.InferExpression where

import Common.State
import Core.Expression
import Core.Term
import Core.Types
import TypeSystem.Common
import TypeSystem.InferOperator
import TypeSystem.InferTerm
import TypeCheck.ConstraintSolver (Constraint (..))

import           Control.Monad   (replicateM)
import           Data.ByteString (ByteString)
import           Data.Functor    ((<&>))
import           Data.Map        (Map)
import qualified Data.Map as M

inferExpr :: Map ByteString (Polytype ByteString)
          -> Expr Untyped ByteString
          -> State (GroupState ByteString) ( [Constraint ByteString]
                                           , Expr (Type ByteString) ByteString )
inferExpr env expr =

    case expr of

        Term Untyped t ->
            inferTerm env t <&> \t' -> ([], t')

        Lam Untyped vs e -> do
            tvs <- replicateM (length vs) freshTVar
            let env' = M.fromList $ zip vs (Forall [] <$> tvs)
            (cs, e') <- inferExpr (env <!> env') e
            let ty = foldr TyArr (typeOf e') tvs
            pure ( cs
                 , Lam ty vs e' )

        App Untyped f xs -> do
            (c1, f')  <-                 inferExpr env  f
            (cs, xs') <- unzip <$> mapM (inferExpr env) xs
            tv        <- freshTVar
            let ts = map typeOf xs'
            let t1 = typeOf f'
            pure ( c1 ++ concat cs ++ [Constraint t1 (foldr TyArr tv ts)]
                 , App tv f' xs' )

        Let Untyped a b c -> do
            tv <- freshTVar
            ta <- freshTVar
            let env' = M.insert a (Forall [] ta) env
            (csb, b') <- inferExpr env' b
            (csc, c') <- inferExpr env' c
            pure ( Constraint ta (typeOf b')
                 : Constraint tv (typeOf c')
                 : csb ++ csc
                 , Let tv a b' c' )

        UnPrimOp Untyped op e -> do
            (c, e') <- inferExpr env e
            tv      <- freshTVar
            tOp     <- unOp op
            pure ( c ++ [Constraint (typeOf e' `TyArr` tv) tOp]
                 , UnPrimOp tv op e' )

        BinPrimOp Untyped op e1 e2 -> do
            (c1, e1') <- inferExpr env e1
            (c2, e2') <- inferExpr env e2
            tv        <- freshTVar
            tOp       <- binOp op
            pure ( c1 ++ c2 ++ [Constraint (typeOf e1' `TyArr` (typeOf e2' `TyArr` tv)) tOp]
                 , BinPrimOp tv op e1' e2' )

        IfThenElse Untyped p tr fl -> do
            (c1, p')  <- inferExpr env p
            (c2, tr') <- inferExpr env tr
            (c3, fl') <- inferExpr env fl
            pure ( c1 ++ c2 ++ c3 ++ [ Constraint (typeOf p') typeBool
                                     , Constraint (typeOf tr') (typeOf fl')]
                 , IfThenElse (typeOf tr') p' tr' fl' )

        Case Untyped scrut ps -> do

            tv           <- freshTVar
            (cs, scrut') <- inferExpr env scrut
            (pcs, ps')   <- unzip <$> mapM (inferPattern env) ps

            -- get left and right hand types
            let (lts, rts) = unzip $ map (\(Pattern a b) -> (typeOf a, typeOf b)) ps'

            -- the left-side of each pattern must match the scrutinee
            let lhs = map (Constraint (typeOf scrut')) lts

            -- the right-side of each pattern must match the whole type
            let rhs = map (Constraint tv) rts

            pure ( cs ++ concat pcs ++ lhs ++ rhs
                 , Case tv scrut' ps')

inferPattern :: Map ByteString (Polytype ByteString)
             -> Pattern Untyped ByteString
             -> State (GroupState ByteString) ( [Constraint ByteString]
                                              , Pattern (Type ByteString) ByteString )
inferPattern env (Pattern a b) = do
    -- TODO: should probably enforce arity of LHS data constructions
    lhsVars <- fmap (Forall []) <$> labelLeftFreshVars a -- Guess
    (acs, a') <- inferExpr (env <!> lhsVars) a
    (bcs, b') <- inferExpr (env <!> lhsVars) b
    pure ( acs <> bcs
         , Pattern a' b')

(<!>) :: (Ord k, Show v, Eq v) => Map k v -> Map k v -> Map k v
m1 <!> m2 = M.unionWith same m1 m2
    where
    same a b | a == b    = a
             | otherwise = error $ "intersection!: " ++ show (a, b)


labelLeftFreshVars :: Expr Untyped ByteString
                   -> State (GroupState ByteString) (Map ByteString (Type ByteString))
labelLeftFreshVars a =

    case a of

        App Untyped dc xs -> do
            fdc <- labelLeftFreshVars dc
            fxs <- mapM labelLeftFreshVars xs
            pure $ mconcat (fdc:fxs)

        Term Untyped DCons{} ->
            pure mempty

        Term Untyped LitBool{} ->
            pure mempty

        Term Untyped LitInt{} ->
            pure mempty

        Term Untyped LitString{} ->
            pure mempty

        Term Untyped (Var v) ->
            M.singleton v <$> freshTVar

-- TODO dedupe?
varsFrom :: Show s => [Expr Untyped s] -> [s]
varsFrom = go []
    where
    go acc                 [] = acc
    go acc (Term Untyped (Var v):xs) = go (v:acc) xs
