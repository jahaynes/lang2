module Phase.ClosureConvert (closureConvert) where

import Common.State
import Core.Definition
import Core.Expression
import FreeVars.FreeVars

import           Data.ByteString.Char8 (ByteString)
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

newtype ConvState s =
    ConvState { getScope:: Set s }

closureConvert :: Module ByteString -> Module ByteString
closureConvert md = md { getFunDefns = closureConvert' $ getFunDefns md }

closureConvert' :: [FunDefn ByteString] -> [FunDefn ByteString]
closureConvert' funDefs =
    let topLevelScope = S.fromList $ map (\(FunDefn n _) -> n) funDefs
    in fst $ runState (mapM (closureConvertDefn topLevelScope) funDefs) (ConvState mempty)

closureConvertDefn :: Set ByteString
                   -> FunDefn ByteString
                   -> State (ConvState ByteString) (FunDefn ByteString)
closureConvertDefn topLevelScope (FunDefn n fun) =

    case fun of
        ELam vs body -> FunDefn n . ELam vs <$> cc body
        _            -> FunDefn n <$> cc fun

    where
    cc e =
        case e of

            ETerm{} ->
                pure e

            ELam vs body -> do
                body' <- cc body
                ConvState currentScope <- get
                let scope = S.fromList vs <> topLevelScope <> currentScope
                let fvs = S.toList . getFree . snd $ runState (exprFreeVars body') (FreeVars scope mempty)
                pure $ if null fvs
                           then ELam     vs body'
                           else EClo fvs vs body'

            EClo{} ->
                error "doesn't exist yet"

            EApp f xs ->
                EApp <$> cc f
                     <*> mapM cc xs

            ELet a b c -> do
                -- Include a in the scope to allow recursion
                addToScope [a]
                b' <- cc b
                c' <- cc c
                removeFromScope [a]
                pure $ ELet a b' c'

            EUnPrimOp o a ->
                EUnPrimOp o <$> cc a

            EBinPrimOp o a b ->
                EBinPrimOp o <$> cc a
                             <*> cc b

            IfThenElse p t f ->
                IfThenElse <$> cc p
                           <*> cc t
                           <*> cc f

            CallClo{} ->
                error "Doesn't exist yet"

addToScope :: Ord s => [s] -> State (ConvState s) ()
addToScope vs = modify' $ \cs -> cs { getScope = getScope cs <> S.fromList vs }

removeFromScope :: Ord s => [s] -> State (ConvState s) ()
removeFromScope vs = modify' $ \cs -> cs { getScope = getScope cs \\ S.fromList vs }
