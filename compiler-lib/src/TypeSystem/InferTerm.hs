module TypeSystem.InferTerm (inferTerm) where

import Common.State
import Core.Expression
import Core.Term
import Core.Types
import TypeSystem.Common

import           Data.ByteString (ByteString)
import           Data.Functor    ((<&>))
import           Data.Map        (Map)
import qualified Data.Map as M

inferTerm :: Map ByteString (Polytype ByteString)
          -> Term ByteString
          -> State (GroupState ByteString) (ExprT ByteString)
inferTerm env term =
    case term of
        LitBool{}   -> pure $ TermT typeBool term
        LitInt{}    -> pure $ TermT typeInt term
        LitString{} -> pure $ TermT typeString term
        DCons{}     -> error "Data Constructors not implemented"
        Var v       ->
            case M.lookup v env of
                Nothing -> error $ "unbound: " ++ show v
                Just p  -> instantiate p <&> \t -> TermT t term
    where
    instantiate (Forall as t) = do
        as' <- mapM (const freshTVar) as
        let s = Subst $ M.fromList $ zip as as'
        pure $ substituteType s t

newtype Subst s =
    Subst (Map s (Type s))
        deriving Show

substituteType :: Ord s => Subst s -> Type s -> Type s
substituteType         _       (TyCon a) = TyCon a
substituteType (Subst s)     t@(TyVar a) = M.findWithDefault t a s
substituteType         s (t1 `TyArr` t2) = substituteType s t1 `TyArr` substituteType s t2
