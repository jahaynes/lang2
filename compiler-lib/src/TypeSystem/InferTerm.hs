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

-- Todo Either error-handling
inferTerm :: Map ByteString (Polytype ByteString)
          -> Term ByteString
          -> State (GroupState ByteString) (ExprT ByteString)
inferTerm env term =
    case term of
        LitBool{}   -> pure $ TermT typeBool term
        LitInt{}    -> pure $ TermT typeInt term
        LitString{} -> pure $ TermT typeString term
        DCons d     ->
            case M.lookup d env of
                Nothing -> error $ "unbound data cons: " ++ show d
                Just p -> instantiate p <&> \t -> TermT t term
        Var v       ->
            case M.lookup v env of
                Nothing -> error $ "unbound var: " ++ show v
                Just p  -> instantiate p <&> \t -> TermT t term
