module TypeSystem.Common where

import Common.State
import Core.Types

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Char             (chr)
import           Data.Map              (Map)
import qualified Data.Map as M

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

substituteType :: Ord s => Subst s -> Type s -> Type s
substituteType         _       (TyCon a) = TyCon a
substituteType (Subst s)     t@(TyVar a) = M.findWithDefault t a s
substituteType         s (t1 `TyArr` t2) = substituteType s t1 `TyArr` substituteType s t2

instantiate :: Polytype ByteString
            -> State (GroupState s) (Type ByteString)
instantiate (Forall as t) = do
    as' <- mapM (const freshTVar) as
    let s = Subst $ M.fromList $ zip as as'
    pure $ substituteType s t
