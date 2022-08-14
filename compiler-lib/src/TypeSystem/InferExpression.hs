module TypeSystem.InferExpression where

import Common.State
import Core.Expression
import Core.Types
import TypeSystem.Common
import TypeSystem.InferOperator
import TypeSystem.InferTerm
import TypeCheck.ConstraintSolver (Constraint (..))

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
            tvs      <- freshlyLabel vs
            (cs, e') <- inferExpr (env <> (Forall [] <$> tvs)) e
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
            tv        <- freshTVar
            let env' = M.insert a (Forall [] tv) env
            (csb, b') <- inferExpr env' b
            (csc, c') <- inferExpr env' c
            pure ( Constraint tv (typeOf b') : csb ++ csc
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

freshlyLabel :: Ord s => [s]
                      -> State (GroupState ByteString) (Map s (Type ByteString))
freshlyLabel ns = M.fromList <$> mapM (\n -> freshTVar <&> \f -> (n, f)) ns
