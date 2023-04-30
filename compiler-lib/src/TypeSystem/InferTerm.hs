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
import           Debug.Trace (trace)

-- Todo Either error-handling
inferTerm :: Map ByteString (Polytype ByteString)
          -> Term ByteString
          -> State (GroupState ByteString) (ExprT ByteString)
inferTerm env term =
    -- env is: fromList [ ("Yes", Forall ["a"] ("a" -> ("Answer" "a")))
    --                  , ("yes", Forall ["TODO"] "a0")]

    -- "yes" should eventually be: Forall [] (Answer "Int")

    case term of
        LitBool{}   -> pure $ TermT typeBool term
        LitInt{}    -> pure $ TermT typeInt term
        LitString{} -> pure $ TermT typeString term
        DCons d     ->
            case M.lookup d env of
                Nothing -> error $ "unbound data cons: " ++ show d
                Just p ->
                    instantiate p <&> \t ->
                        trace (unlines [ "inferTerm: " ++ show d
                                       , "inferTerm: " ++ show p
                                       , "inferTerm: " ++ show t ]) $ TermT t term

                    {-
                        inferTerm: "Yes"
                        inferTerm: Forall ["a"] ("a" -> ("Answer" "a"))
                        inferTerm: ("b0" -> ("Answer" "a"))                    
                    -}

                    -- This is wrong.
                    -- should it instantiate as:
                    -- inferTerm: ("b0" -> ("Answer" "b0"))         

        Var v       -> -- No problem here
            case M.lookup v env of
                Nothing -> error $ "unbound var: " ++ show v
                Just p  -> instantiate p <&> \t -> TermT t term
