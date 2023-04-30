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
import           Debug.Trace     (trace)

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

    let untypedFunDefs =
            S.map (\n -> FunDefn n (funDefnMap !!! n)) <$> typeCheckPlan

    --let typeSigs = M.fromList
    --             . map (\(TypeSig n t) -> (n, Forall [pack "blARH"] t)) -- TODO
    --             $ getTypeSigs md

    let dataConstructors =
            foldl' dataDefnToType mempty (getDataDefns md)

    let env =
            dataConstructors -- typeSigs <!> dataConstructors

    -- TODO data definitions could accompany typeSigs here
    case foldl' go (Right $ GroupInference 0 env []) untypedFunDefs of
        Left ex -> Left ex
        Right (GroupInference _ _ defs') ->
            let dataDefns = getDataDefns md
            in Right (ModuleT dataDefns defs')

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
    go (DataCon dc xs) = do

        let monotype = foldr TyArr (TyCon typeName (map TyVar tvs)) (map to xs) -- not []
            polytype = Forall tvs monotype

        trace ("val: " <> show polytype) (dc, polytype)

        where
        to (MemberVar v)     = TyVar v
        to (MemberType t ts) = --TyCon t [] -- Not [] ! 
            let ts' = map to ts
            in foldr TyArr (TyCon t []) ts' -- TODO is this necessary/right?

data GroupInference =
    GroupInference !Int
                   !(Map ByteString (Polytype ByteString))
                   ![FunDefnT ByteString]


(!!!) m k =
    case M.lookup k m of
        Nothing -> error $ "Couldn't find " ++ show k ++ " in " ++ show m
        Just x  -> x

inferGroup :: Map ByteString (Polytype ByteString)
           -> Set (FunDefn ByteString)
           -> State (GroupState ByteString)
                    (Either ByteString GroupInference)

inferGroup env untyped = do

    -- env is: fromList [("Yes",Forall ["a"] ("a" -> ("Answer" "a")))]

    let namesAndExprs = map (\(FunDefn n e) -> (n, e)) $ S.toList untyped

    -- TODO merge the following 2 steps (why instantiate to immediately generalise)

    -- 1
    -- Assign provided type-sig, or generate fresh one
    topLevelTypes <- fromEnvOrFresh env (map fst namesAndExprs)
    -- topLevelTypes: fromList [(\"yes\",\"a0\")]
    -- This is fine.  a0 just means we don't know yet

    currentState <- get
    -- currentState is: GroupState {getVarNum = 1, getConstraints = []}"

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

    {-  inferredConstraints: [ Constraint (TyVar "a0")
                                          (TyVar "d0")

                             , Constraint (TyArr (TyVar "b0")     (TyArr (TyArr (TyVar "b0") (TyCon "List" [])) (TyCon "List" [TyVar "b0"])))
                                          (TyArr (TyCon "Int" []) (TyArr (TyCon "List" [TyVar "c0"]) (TyVar "d0")))
                             ]
    -}

    trace ("inferredConstraints: " ++ show inferredConstraints) $ do

      -- Deduce the types from the constraints
      case runSolve inferredConstraints of

        Left e -> pure $ Left e

        Right sub -> do

            -- Define them as functions
            let funDefnTs = map (cleanup sub) inferredTypedDefs

            -- Put the newly-inferred types into the enviromment
            let env'' = M.fromList
                      . map (\(FunDefnT n (Quant vs) expr) -> (n, Forall vs (typeOf expr)))
                      $ funDefnTs

            n' <- getVarNum <$> get

            pure . Right $ GroupInference n' env'' funDefnTs

fromEnvOrFresh :: Show s => Map ByteString (Polytype ByteString)
               -> [ByteString]
               -> State (GroupState s) (Map ByteString (Type ByteString))
fromEnvOrFresh env names =
    -- Names are: ["yes"]
    M.fromList <$> mapM go names
    where
    go n =
        case M.lookup n env of
            Nothing -> freshTVar      <&> \f -> (n, f)
            Just pt -> instantiate pt <&> \t -> (n, t)

cleanup :: Subst ByteString
        -> (ByteString, ExprT ByteString)
        -> FunDefnT ByteString
cleanup subst (name, expr) =

    -- Substitute type metavariables for types
    let expr' = mapType (substituteType subst) expr

    in norm $ generaliseTopLevel name expr'

norm :: FunDefnT ByteString -> FunDefnT ByteString
norm (FunDefnT name (Quant qs) expr) =
    let normed = take (length qs) . map numToVar $ [0..]
        sub = M.fromList $ zip qs normed
    in FunDefnT name (Quant normed) (mapType (go sub) expr)
    where
    go   _ t@TyCon{}   = t
    go sub (TyVar v)   = TyVar (sub !!! v)
    go sub (TyArr a b) = TyArr (go sub a) (go sub b)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)