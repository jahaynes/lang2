module Phase.ClosureConvertTyped (closureConvert) where

import Common.State
import Core.Expression
import Core.Module
import Core.Types
import FreeVars.FreeVars

import           Data.ByteString.Char8 (ByteString)
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

newtype ConvState s =
    ConvState { getScope :: Set s }

closureConvert :: TypedModule ByteString -> TypedModule ByteString
closureConvert md = md { getTFunDefns = closureConvert' $ getTFunDefns md }

closureConvert' :: [TFunDefn ByteString] -> [TFunDefn ByteString]
closureConvert' funDefs =
    let topLevelScope = S.fromList $ map (\(TFunDefn n _) -> n) funDefs
    in evalState (mapM (closureConvertDefn topLevelScope) funDefs) (ConvState mempty)

closureConvertDefn :: Set ByteString
                   -> TFunDefn ByteString
                   -> State (ConvState ByteString) (TFunDefn ByteString)
closureConvertDefn topLevelScope (TFunDefn n fun) =

    case fun of
        ALam t vs body -> TFunDefn n . ALam t vs <$> cc body
        _              -> TFunDefn n <$> cc fun

    where
    cc :: AExpr (Polytype ByteString) ByteString
       -> State (ConvState ByteString) (AExpr (Polytype ByteString) ByteString)
    cc e =
        case e of

            ATerm{} ->
                pure e

            ALam t vs body -> do
                body' <- cc body
                ConvState currentScope <- get
                let scope = S.fromList vs <> topLevelScope <> currentScope
                let fvs = S.toList . getFree . snd $ runState (typedExprFreeVars body') (FreeVars scope mempty)
                pure $ if null fvs
                           then ALam t     vs body'
                           else AClo t fvs vs body'

            AClo{} ->
                error "doesn't exist yet"

            AApp t f xs ->
                AApp t <$> cc f
                       <*> mapM cc xs

            ALet t a b c -> do
                -- Include a in the scope to allow recursion
                addToScope [a]
                b' <- cc b
                c' <- cc c
                removeFromScope [a]
                pure $ ALet t a b' c'

            AUnPrimOp t o a ->
                AUnPrimOp t o <$> cc a

            ABinPrimOp t o a b ->
                ABinPrimOp t o <$> cc a
                               <*> cc b

            AIfThenElse t p tr fl ->
                AIfThenElse t <$> cc p
                              <*> cc tr
                              <*> cc fl

            ACallClo{} ->
                error "Doesn't exist yet"

addToScope :: Ord s => [s] -> State (ConvState s) ()
addToScope vs = modify' $ \cs -> cs { getScope = getScope cs <> S.fromList vs }

removeFromScope :: Ord s => [s] -> State (ConvState s) ()
removeFromScope vs = modify' $ \cs -> cs { getScope = getScope cs \\ S.fromList vs }
