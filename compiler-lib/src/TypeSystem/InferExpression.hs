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
          -> Expr ByteString
          -> State (GroupState ByteString) ([Constraint ByteString], ExprT ByteString)
inferExpr env expr =

    case expr of

        ETerm t ->
            inferTerm env t <&> \t' -> ([], t')

        ELam vs e -> do
            tvs <- replicateM (length vs) freshTVar
            let env' = M.fromList $ zip vs (Forall [] <$> tvs)
            (cs, e') <- inferExpr (env <> env') e
            let ty = foldr TyArr (typeOf e') tvs
            pure ( cs
                 , LamT ty vs e' )

        EApp f xs -> do
            (c1, f')  <-                 inferExpr env  f
            (cs, xs') <- unzip <$> mapM (inferExpr env) xs
            tv        <- freshTVar
            let ts = map typeOf xs'
            let t1 = typeOf f'
            pure ( c1 ++ concat cs ++ [Constraint t1 (foldr TyArr tv ts)]
                 , AppT tv f' xs' )

        ELet a b c -> do
            tv <- freshTVar
            ta <- freshTVar
            let env' = M.insert a (Forall [] ta) env
            (csb, b') <- inferExpr env' b
            (csc, c') <- inferExpr env' c
            pure ( Constraint ta (typeOf b')
                 : Constraint tv (typeOf c')
                 : csb ++ csc
                 , LetT tv a b' c' )

        EUnPrimOp op e -> do
            (c, e') <- inferExpr env e
            tv      <- freshTVar
            tOp     <- unOp op
            pure ( c ++ [Constraint (typeOf e' `TyArr` tv) tOp]
                 , UnPrimOpT tv op e' )

        EBinPrimOp op e1 e2 -> do
            (c1, e1') <- inferExpr env e1
            (c2, e2') <- inferExpr env e2
            tv        <- freshTVar
            tOp       <- binOp op
            pure ( c1 ++ c2 ++ [Constraint (typeOf e1' `TyArr` (typeOf e2' `TyArr` tv)) tOp]
                 , BinPrimOpT tv op e1' e2' )

        IfThenElse p tr fl -> do
            (c1, p')  <- inferExpr env p
            (c2, tr') <- inferExpr env tr
            (c3, fl') <- inferExpr env fl
            pure ( c1 ++ c2 ++ c3 ++ [ Constraint (typeOf p') typeBool
                                     , Constraint (typeOf tr') (typeOf fl')]
                 , IfThenElseT (typeOf tr') p' tr' fl' )

        ECase scrut ps -> do

            tv                <- freshTVar
            (cs, scrut')      <- inferExpr env scrut
            (clhs, crhs, ps') <- unzip3 <$> mapM (inferPattern env) ps

            -- get left and right hand types
            let (lts, rts) = unzip $ map (\(PatternT a b) -> (typeOf a, typeOf b)) ps'

            -- the left-side of each pattern must match the scrutinee
            let lhs = map (Constraint (typeOf scrut')) lts

            -- the right-side of each pattern must match the whole type
            let rhs = map (Constraint tv) rts

            pure ( cs ++ concat clhs ++ concat crhs ++ lhs ++ rhs
                 , CaseT tv scrut' ps')

inferPattern :: Map ByteString (Polytype ByteString)
             -> Pattern ByteString
             -> State (GroupState ByteString) ( [Constraint ByteString]
                                              , [Constraint ByteString]
                                              , PatternT ByteString
                                              )
inferPattern env (Pattern a b) = do
    env' <- labelLeftFreshVars a
    (clhs, a') <- inferExpr (env <> env') a
    (crhs, b') <- inferExpr (env <> env') b
    pure ( clhs
         , crhs
         , PatternT a' b' )

labelLeftFreshVars :: Expr ByteString
                   -> State (GroupState ByteString) (Map ByteString (Polytype ByteString))
labelLeftFreshVars a =

    case a of

        EApp (ETerm DCons{}) xs ->
            -- TODO guessed ForAll
            M.fromList <$> mapM (\x -> freshTVar <&> \fr -> (x, Forall [] fr)) (varsFrom xs)

        ETerm DCons{} ->
            pure mempty

        ETerm LitBool{} ->
            pure mempty

        ETerm LitInt{} ->
            pure mempty

        ETerm LitString{} ->
            pure mempty

        ETerm (Var v) -> do
            -- TODO guessed ForAll
            fv <- freshTVar
            pure $ M.singleton v (Forall [] fv)

-- TODO dedupe?
varsFrom :: Show s => [Expr s] -> [s]
varsFrom = go []
    where
    go acc                 [] = acc
    go acc (ETerm (Var v):xs) = go (v:acc) xs
