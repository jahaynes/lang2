{-# LANGUAGE ScopedTypeVariables #-}

module TypeSystem.TypeCheck (inferModule) where

import Common.CallGraph
import Common.State
import Core.Expression
import Core.Module
import Core.Types
import TypeCheck.ConstraintSolver
import TypeSystem.Common
import TypeSystem.InferExpression

import           Control.Monad   (forM)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Functor    ((<&>))
import           Data.List       (foldl')
import           Data.Map        (Map)
import           Data.Set        (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data ModuleState t s =
    ModuleState { getEnv     :: Map s (Polytype s)
                , getUntyped :: [Set (FunDefn t s)]
                } deriving Show

inferModule :: Module Untyped ByteString
            -> Either ByteString (Module (Type ByteString) ByteString)
inferModule md = do

    let funDefnMap = M.fromList
                   . map (\(FunDefn n _ e) -> (n, e))
                   $ getFunDefns md

    typeCheckPlan <- planExcludingPretyped md (buildGraph md)

    let untypedFunDefs =
            S.map (\n -> FunDefn n (Quant []) (funDefnMap !!! n)) <$> typeCheckPlan

    let typeSigs = getTypeSigs md

    let typeSigs' = M.fromList
                  . map (\(TypeSig n t) -> (n, Forall [pack "TODO"] t))
                  $ typeSigs

    -- TODO, either return the inferred typesigs or warn if they don't match the provided ones

    let dataConstructors =
            foldl' dataDefnToType mempty (getDataDefns md)

    let env =
            typeSigs' <!> dataConstructors

    -- TODO data definitions could accompany typeSigs here
    case foldl' go (Right $ GroupInference 0 env []) untypedFunDefs of
        Left ex -> Left ex
        Right (GroupInference _ _ defs') ->
            let dataDefns = getDataDefns md
            in Right (Module dataDefns typeSigs defs')

    where
    go eGroupInference un =

        case eGroupInference of

            Left ex -> Left ex

            Right (GroupInference n e fds) ->

                case evalState (inferGroup e un) (GroupState n mempty) of

                    Left ex -> Left ex

                    Right (GroupInference n' e' fds') ->
                        Right $ GroupInference n' (e <!> e') (fds ++ fds')

-- TODO polymorph properly!
-- untested
dataDefnToType :: (Ord s, Show s) => Map s (Polytype s)
                                  -> DataDefn s
                                  -> Map s (Polytype s)
dataDefnToType env (DataDefn typeName tvs dcons) =

    env <!> M.fromList (map go dcons)

    where
    go (DataCon dc xs) =
        let monotype = foldr TyArr (TyCon typeName (map TyVar tvs)) (map to xs)
        in (dc, Forall tvs monotype)

        where
        to (MemberVar v)     = TyVar v
        to (MemberType t ts) = TyCon t (map to ts)

data GroupInference =
    GroupInference !Int
                   !(Map ByteString (Polytype ByteString))
                   ![FunDefn (Type ByteString) ByteString]

(!!!) :: (Ord k, Show k, Show v) => Map k v -> k -> v
(!!!) m k =
    case M.lookup k m of
        Nothing -> error $ "Couldn't find " ++ show k ++ " in " ++ show m
        Just x  -> x

inferGroup :: Map ByteString (Polytype ByteString)
           -> Set (FunDefn Untyped ByteString)
           -> State (GroupState ByteString)
                    (Either ByteString GroupInference)

inferGroup env untyped = do

    let namesAndExprs = map (\(FunDefn n _ e) -> (n, e)) $ S.toList untyped

    -- TODO merge the following 2 steps (why instantiate to immediately generalise)

    -- 1
    -- Assign provided type-sig, or generate fresh one
    topLevelTypes <- fromEnvOrFresh env (map fst namesAndExprs)

    -- 2
    -- Make them available in the environment
    -- (allows recursion)
    let env' = env <!> (Forall [pack "TODO"] <$> topLevelTypes) -- WHAT?

    -- Infer each expression and gather the constraints
    inferences <- forM namesAndExprs (\(n, expr) -> do
        (cs, tex) <- inferExpr env' expr
        -- Constrain the inferred type to the declared type
        let c = Constraint (topLevelTypes !!! n) (typeOf tex)
        pure (c:cs, (n, tex)))

    let (inferredConstraints, inferredTypedDefs) =
            first concat $ unzip inferences

    -- Deduce the types from the constraints
    case runSolve inferredConstraints of

        Left e -> pure $ Left e

        Right sub -> do

            -- Define them as functions
            let funDefns = map (cleanup sub) inferredTypedDefs

            -- Put the newly-inferred types into the enviromment
            let env'' = M.fromList
                      . map (\(FunDefn n (Quant vs) expr) -> (n, Forall vs (typeOf expr)))
                      $ funDefns

            n' <- getVarNum <$> get

            pure . Right $ GroupInference n' env'' funDefns

fromEnvOrFresh :: Show s => Map ByteString (Polytype ByteString)
               -> [ByteString]
               -> State (GroupState s) (Map ByteString (Type ByteString))
fromEnvOrFresh env names =
    M.fromList <$> mapM go names
    where
    go n =
        case M.lookup n env of
            Nothing -> freshTVar      <&> \f -> (n, f)
            Just pt -> instantiate pt <&> \t -> (n, t)

cleanup :: Subst ByteString
        -> (ByteString, Expr (Type ByteString) ByteString)
        -> FunDefn (Type ByteString) ByteString
cleanup subst (name, expr) =

    -- Substitute type metavariables for types
    let expr' = mapType (substituteType subst) expr

    in norm $ generaliseTopLevel name expr'

norm :: FunDefn (Type ByteString) ByteString
     -> FunDefn (Type ByteString) ByteString
norm (FunDefn name (Quant qs) expr) =
    let normed = take (length qs) . map numToVar $ [0..]
        sub = M.fromList $ zip qs normed
    in FunDefn name (Quant normed) (mapType (go sub) expr)
    where
    go sub (TyCon tc ts) = TyCon tc (map (go sub) ts)
    go sub (TyVar v)     = TyVar (sub !!! v)
    go sub (TyArr a b)   = TyArr (go sub a) (go sub b)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)