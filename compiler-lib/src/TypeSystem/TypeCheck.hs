module TypeSystem.TypeCheck (inferModule) where

import Common.CallGraph
import Common.State
import Core.Expression
import Core.Module
import Core.Types
import TypeCheck.ConstraintSolver
import TypeSystem.Common
import TypeSystem.Ftv
import TypeSystem.InferExpression

import           Control.Monad   (forM)
import           Data.ByteString (ByteString)
import           Data.Functor    ((<&>))
import           Data.List       (foldl')
import           Data.Map        ((!), Map)
import           Data.Set        (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data ModuleState s =
    ModuleState { getEnv     :: Map s (Polytype s)
                , getUntyped :: [Set (FunDefn s)]
                } deriving Show

inferModule :: Module ByteString
            -> Either ByteString (ModuleT ByteString)
inferModule md = do

    let funDefnMap = M.fromList
                   . map (\(FunDefn n e) -> (n, e))
                   $ getFunDefns md

    typeCheckPlan <- planExcludingPretyped md (buildGraph md)

    let untyped = S.map (\n -> FunDefn n (funDefnMap ! n)) <$> typeCheckPlan

    let typeSigs = M.fromList . map (\(TypeSig n t) -> (n, Forall [] t)) $ getTypeSigs md

    -- TODO data definitions could accompany typeSigs here
    let (_, _, defs') = foldl' go (0, typeSigs, []) untyped

    Right (ModuleT defs')

    where
    go (n, e, fds) un =
        let (n', e', fds') = evalState (inferGroup e un) (GroupState n mempty)
        in (n', e <> e', fds <> fds')

inferGroup :: Map ByteString (Polytype ByteString)
           -> Set (FunDefn ByteString)
           -> State (GroupState ByteString)
                    (Int
                    , Map ByteString (Polytype ByteString)
                    , [FunDefnT ByteString] )

inferGroup env untyped = do

    let namesAndExprs = map (\(FunDefn n e) -> (n, e)) $ S.toList untyped

    -- Generate a fresh type var for each top-level
    freshlyLabelled <- freshlyLabel (fst <$> namesAndExprs)

    -- Make them available in the environment
    let env' = env <> (Forall [] <$> freshlyLabelled)

    -- Infer each expression and gather the constraints
    inferences <- forM namesAndExprs (\(n, expr) -> do
        (cs, tex) <- inferExpr env' expr
        pure (cs, (n, tex)))

    -- Deduce the types from the constraints
    let Right sub = runSolve $ concatMap fst inferences

    -- Define them as functions
    let funDefnTs = cleanup sub env' . snd <$> inferences

    -- Put the newly-inferred types into the enviromment
    let env'' = M.fromList . map (\(FunDefnT n (Quant vs) expr) -> (n, Forall vs (typeOf expr))) $ funDefnTs

    n' <- getVarNum <$> get

    pure (n', env'', funDefnTs)

    where
    freshlyLabel ns = M.fromList <$> mapM (\n -> freshTVar <&> \f -> (n, f)) ns

cleanup :: Subst ByteString
        -> Map ByteString (Polytype ByteString)
        -> (ByteString, ExprT ByteString)
        -> FunDefnT ByteString
cleanup subst env (name, expr) = do

    -- Substitute type metavariables for types
    let expr' = mapType (substituteType subst) expr

    -- Polymorphise
    let t = typeOf expr'
    let q = Quant . S.toList $ ftvType t `S.difference` ftvEnv env

    -- Normalise
    norm $ FunDefnT name q expr'

norm :: FunDefnT ByteString -> FunDefnT ByteString
norm (FunDefnT name (Quant qs) expr) =
    let normed = take (length qs) . map numToVar $ [0..]
        sub = M.fromList $ zip qs normed
    in FunDefnT name (Quant normed) (mapType (go sub) expr)
    where
    go   _ t@TyCon{} = t
    go sub (TyVar v) = TyVar (sub ! v)
    go sub (TyArr a b) = TyArr (go sub a) (go sub b)
