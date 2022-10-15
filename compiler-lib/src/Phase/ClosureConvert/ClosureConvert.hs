module Phase.ClosureConvert.ClosureConvert where

import Common.State
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule             (AnfModule (..), FunDefAnfT (..))
import Phase.ClosureConvert.FreeVars

import           Data.ByteString       (ByteString)
import           Data.Set              (Set)
import qualified Data.Set as S

closureConvert :: AnfModule ByteString -> AnfModule ByteString
closureConvert md = md { getFunDefAnfTs = closureConvert' $ getFunDefAnfTs md }

closureConvert' :: [FunDefAnfT ByteString] -> [FunDefAnfT ByteString]
closureConvert' funDefs =
    let topLevelScope = S.fromList $ map (\(FunDefAnfT n _ _) -> n) funDefs
    in evalState (mapM (closureConvertDefn topLevelScope) funDefs) (Scope mempty)

closureConvertDefn :: (Ord s, Show s) => Set s
                                      -> FunDefAnfT s
                                      -> State (Scope s) (FunDefAnfT s)
closureConvertDefn topLevelScope (FunDefAnfT n q fun) = do

    fun' <- case fun of
                AExp (ALam t vs body) -> AExp . ALam t vs <$> go body
                _                     -> go fun

    pure $ FunDefAnfT n q fun'

    where
    go expr =

        case expr of

            CExp cexp ->
                CExp <$> ccc cexp

            AExp aexp ->
                AExp <$> cca aexp

            -- Includes a in the scope to allow recursion
            NLet a (AExp (ALam t vs body)) c ->
                withScope [a] $
                    NLet a <$> (AExp <$> cclam (Just a) t vs body)
                           <*> go c

            -- Includes a in the scope to allow recursion
            NLet a b c ->
                withScope [a] $
                    NLet a <$> go b
                           <*> go c

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

                CCase scrut ps ->
                    CCase <$> cca scrut
                          <*> mapM ccp ps

        ccp (PExp a b) =
            PExp <$> go a <*> go b

        cca aexp =

            case aexp of

                t@ATerm{} ->
                    pure t

                ALam t vs body ->
                    cclam Nothing t vs body

                AClo{} ->
                    error "Doesn't exist yet"

                AUnPrimOp t o a ->
                    AUnPrimOp t o <$> cca a

                ABinPrimOp t o a b ->
                    ABinPrimOp t o <$> cca a
                                   <*> cca b

        -- assumes type stays same
        cclam mName t vs body = do
            body' <- go body
            let scope = maybe mempty S.singleton mName <> S.fromList vs <> topLevelScope
                fvs = S.toList $ getFreeVars scope body'
            pure $ if null fvs
                    then ALam t     vs body'
                    else AClo t  fvs vs body'
