{-# LANGUAGE OverloadedStrings #-}

module Phase.ClosureConvert.ClosureConvert where

import Common.State
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule             (AnfModule (..), FunDefAnfT (..))
import Phase.ClosureConvert.FreeVars

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Set              (Set)
import qualified Data.Set as S

closureConvert :: AnfModule ByteString -> AnfModule ByteString
closureConvert md = md { getFunDefAnfTs = closureConvert' $ getFunDefAnfTs md }

closureConvert' :: [FunDefAnfT ByteString] -> [FunDefAnfT ByteString]
closureConvert' funDefs =
    let topLevelScope = S.fromList $ map (\(FunDefAnfT n _ _) -> n) funDefs
    in evalState (mapM (closureConvertDefn genNameImpl topLevelScope) funDefs) (0, Scope mempty)

closureConvertDefn :: (Ord s, Show s) => State (Int, Scope s) s
                                      -> Set s
                                      -> FunDefAnfT s
                                      -> State (Int, Scope s) (FunDefAnfT s)
closureConvertDefn genName topLevelScope (FunDefAnfT n q fun) = do

    fun' <- case fun of
                AExp (ALam t vs body) -> AExp . ALam t vs <$> go body
                _                     -> go fun

    pure $ FunDefAnfT n q fun'

    where
    go expr =

        case expr of

            CExp cexp ->
                CExp <$> ccc cexp

            AExp aexp -> do

                aexp' <- cca aexp

                case aexp' of
                    AClo _ fvs _ _ -> do
                        v <- genName
                        let t = typeOfAExp aexp'
                        let app = CExp $ CAppClo t aexp' (AClosEnv fvs) [] -- always empty?
                        pure $ NLet v app (AExp $ ATerm t (Var v))
                    _ -> pure $ AExp aexp'

            NLet a (AExp (ALam t vs body)) c -> withScope' a $ do
                aexp' <- cclam (Just a) t vs body
                case aexp' of
                    AClo _ fvs _ _ ->
                        NLet a (CExp $ CAppClo t aexp' (AClosEnv fvs) []) <$> go c
                    _  ->
                        NLet a (AExp aexp')                               <$> go c

            NLet a b c ->
                withScope' a $
                    NLet a <$> go b
                           <*> go c

        where
        ccc cexp =

            case cexp of

                CApp t f xs ->
                    CApp t <$> cca f
                           <*> mapM cca xs

                CIfThenElse t pr tr fl ->
                    CIfThenElse t <$> cca pr
                                  <*> go tr
                                  <*> go fl

                CCase t scrut ps ->
                    CCase t <$> cca scrut
                            <*> mapM ccp ps

        ccp (PExp a b) =
            PExp <$> go a <*> go b

        cca aexp =

            case aexp of

                ATerm{} ->
                    pure aexp

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
                    else AClo t fvs vs body'

-- Just for a little ANF transform for the newly introduced closure-call
genNameImpl :: State (Int, a) ByteString
genNameImpl = do
    (n, x) <- get
    put $! (n+1, x)
    pure $ "ccanf_" <> pack (show n)

withScope' :: Ord s => s
                    -> State (Int, Scope s) a
                    -> State (Int, Scope s) a
withScope' v f = do

    -- Save the initial scope
    i@(n, Scope initScope) <- get

    -- Use the new scope for the action
    put (n, Scope (S.insert v initScope))
    x <- f

    -- Restore the old scope
    put i
    pure x
