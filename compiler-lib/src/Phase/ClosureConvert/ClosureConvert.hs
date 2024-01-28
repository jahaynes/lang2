{-# LANGUAGE OverloadedStrings #-}

module Phase.ClosureConvert.ClosureConvert ( closureConvert ) where

import Common.EitherT
import Common.State
import Common.Trans
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule             (AnfModule (..), FunDefAnfT (..))
import Phase.ClosureConvert.FreeVars

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Functor          ((<&>))
import           Data.Set              (Set)
import qualified Data.Set as S

type Cc s a =
    EitherT ByteString (
        State (Int, Scope s)) a

closureConvert :: AnfModule ByteString -> Either ByteString (AnfModule ByteString)
closureConvert md = do

    let funDefs       = getFunDefAnfTs md
        topLevelScope = S.fromList $ map (\(FunDefAnfT n _ _) -> n) funDefs
        funDefMs      = mapM (closureConvertDefn (lift genNameImpl) topLevelScope) funDefs
    
    evalState (runEitherT funDefMs) (0, Scope mempty) <&> \fs ->
        md { getFunDefAnfTs = fs }

closureConvertDefn :: (Ord s, Show s) => Cc s s
                                      -> Set s
                                      -> FunDefAnfT s
                                      -> Cc s (FunDefAnfT s)
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
                        let app = CExp $ CAppClo t aexp' (AClosEnv fvs) [] -- always empty? -- put this in a writer for warnings
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

                CAppClo{} ->
                    left "Closure Apps shouldn't exist here yet"

        -- Assume no work needed LHS
        ccp (PExp a b) =
            PExp a <$> go b

        cca aexp =

            case aexp of

                ATerm{} ->
                    pure aexp

                ALam t vs body ->
                    cclam Nothing t vs body

                AClo{} ->
                    left "Closures shouldn't exist here yet"

                AUnPrimOp t o a ->
                    AUnPrimOp t o <$> cca a

                ABinPrimOp t o a b ->
                    ABinPrimOp t o <$> cca a
                                   <*> cca b

        -- assumes type stays same
        cclam mName t vs body = do
            body' <- go body
            let scope = maybe mempty S.singleton mName <> S.fromList vs <> topLevelScope
            pure $ case S.toList <$> getFreeVars scope body' of
                Left er -> error $ show er -- TODO handle
                Right fvs
                    | null fvs  -> ALam t     vs body'
                    | otherwise -> AClo t fvs vs body'

-- Just for a little ANF transform for the newly introduced closure-call
genNameImpl :: State (Int, a) ByteString
genNameImpl = do
    (n, x) <- get
    put $! (n+1, x)
    pure $ "ccanf_" <> pack (show n)

withScope' :: Ord s => s
                    -> Cc s a
                    -> Cc s a
withScope' v f = do

    -- Save the initial scope
    i@(n, Scope initScope) <- lift get

    -- Use the new scope for the action
    lift $ put (n, Scope (S.insert v initScope))
    x <- f

    -- Restore the old scope
    lift $ put i
    pure x
