module TypeSystem.Common where

import Common.State
import Core.Expression
import Core.Module
import Core.Types

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Char             (chr)
import           Data.Map              (Map)
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S

data GroupState s =
    GroupState { getVarNum      :: Int
               , getConstraints :: [(s, s)]
               } deriving Show

newtype Subst s =
    Subst (Map s (Type s))
        deriving Show

freshTVar :: State (GroupState s) (Type ByteString)
freshTVar = do
    gs <- get
    let n = getVarNum gs
    put gs { getVarNum = n + 1 }
    pure . TyVar $ numToVar n

numToVar :: Int -> ByteString
numToVar n =
    let (num, letter) = n `divMod` 26
    in pack (chr (letter + 97) : show num)

substituteType :: (Show s, Ord s) => Subst s -> Type s -> Type s
substituteType         s   (TyCon a tvs) = TyCon a (map (substituteType s) tvs)
substituteType (Subst s)     t@(TyVar a) = M.findWithDefault t a s
substituteType         s (t1 `TyArr` t2) = substituteType s t1 `TyArr` substituteType s t2

instantiate :: Show s => Polytype ByteString
            -> State (GroupState s) (Type ByteString)
instantiate (Forall as t) = do
    as' <- mapM (const freshTVar) as
    let s = Subst $ M.fromList $ zip as as'
    pure $ substituteType s t

-- is this the wrong way to find all the free type vars?
-- (only inspects the top-level type)
generaliseTopLevel :: (Ord s, Show s) => s
                                      -> Expr (Type s) s
                                      -> FunDefnT (Type s) s
generaliseTopLevel name expr =
    let fv = S.toList $ typeVars expr
    in FunDefnT name (Quant fv) expr

-- stick elsewhere?
typeVars :: Ord s => Expr (Type s) s -> Set s
typeVars e =

    case e of

        Term t _ ->
            typeVars' t

        Lam t _ b ->
            typeVars' t <> typeVars b

        App t f xs ->
            typeVars' t <> typeVars f <> mconcat (map typeVars xs)

        Let t _ b c ->
            typeVars' t <> typeVars b <> typeVars c

        UnPrimOp t _ a ->
            typeVars' t <> typeVars a

        BinPrimOp t _ a b  ->
            typeVars' t <> typeVars a <> typeVars b

        IfThenElse t pr tr fl ->
            typeVars' t <> mconcat (map typeVars [pr, tr, fl])

        Case t scrut ps ->
            typeVars' t <> typeVars scrut <> mconcat (map typeVars'' ps)

typeVars'' :: Ord s => Pattern (Type s) s -> Set s
typeVars'' (Pattern a b) = typeVars a <> typeVars b

typeVars' :: Ord s => Type s -> Set s
typeVars'   TyCon{}   = mempty
typeVars' (TyVar v)   = S.singleton v
typeVars' (TyArr a b) = typeVars' a <> typeVars' b