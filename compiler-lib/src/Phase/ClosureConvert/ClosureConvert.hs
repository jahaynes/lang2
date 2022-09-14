module Phase.ClosureConvert.ClosureConvert where

import Common.State
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule           (AnfModule (..), FunDefAnfT (..))
import Phase.ClosureConvert.FreeVars

import           Data.ByteString       (ByteString)
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

newtype ConvState s =
    ConvState { getScope:: Set s }

closureConvert :: AnfModule ByteString -> AnfModule ByteString
closureConvert md = md { getFunDefAnfTs = closureConvert' $ getFunDefAnfTs md }

closureConvert' :: [FunDefAnfT ByteString] -> [FunDefAnfT ByteString]
closureConvert' funDefs =
    let topLevelScope = S.fromList $ map (\(FunDefAnfT n _ _) -> n) funDefs
    in evalState (mapM (closureConvertDefn topLevelScope) funDefs) (ConvState mempty)

closureConvertDefn :: Set ByteString
                   -> FunDefAnfT ByteString
                   -> State (ConvState ByteString) (FunDefAnfT ByteString)
closureConvertDefn topLevelScope (FunDefAnfT n q fun) =

    case fun of
            AExp (ALam vs body) ->
                FunDefAnfT n q . AExp . ALam vs <$> go body
            _ ->
                FunDefAnfT n q <$> go fun

    where
    go expr =

        case expr of
            CExp cexp  -> CExp <$> ccc cexp
            AExp aexp  -> AExp <$> cca aexp
            NLet a b c -> do
                -- Include a in the scope to allow recursion
                addToScope [a]
                b' <- go b
                c' <- go c
                removeFromScope [a]
                pure $ NLet a b' c'

        where
        ccc cexp =

            case cexp of

                CApp f xs ->
                    CApp <$> cca f
                        <*> mapM cca xs

                CIfThenElse pr tr fl ->
                    CIfThenElse <$> cca pr
                                <*> go tr
                                <*> go fl

        cca aexp =

            case aexp of

                t@ATerm{} -> pure t

                ALam vs body -> do
                    body' <- go body
                    ConvState currentScope <- get
                    let scope = S.fromList vs <> topLevelScope <> currentScope
                    let fvs = S.toList . getFree . snd $ runState (nexpFreeVars body') (FreeVars scope mempty)
                    pure $ if null fvs
                            then ALam     vs body'
                            else AClo fvs vs body'

                AClo{} -> error "Doesn't exist yet"

                ABinPrimOp o a b ->
                    ABinPrimOp o <$> cca a
                                 <*> cca b

                AUnPrimOp o a ->
                    AUnPrimOp o <$> cca a

addToScope :: Ord s => [s] -> State (ConvState s) ()
addToScope vs = modify' $ \cs -> cs { getScope = getScope cs <> S.fromList vs }

removeFromScope :: Ord s => [s] -> State (ConvState s) ()
removeFromScope vs = modify' $ \cs -> cs { getScope = getScope cs \\ S.fromList vs }
