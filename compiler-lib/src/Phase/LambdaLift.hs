{-# LANGUAGE OverloadedStrings #-}

module Phase.LambdaLift (lambdaLift) where

import Common.State
--import Core.Expression
--import Core.Module
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.String           (IsString)
import qualified Data.Map as M

data LiftState s =
    LiftState { liftedNum    :: !Int
              , lifted       :: ![FunDefAnfT s]
              } deriving Show

lambdaLift :: AnfModule ByteString
           -> AnfModule ByteString
lambdaLift md =
    md { getFunDefAnfTs = lambdaLift' $ getFunDefAnfTs md }

lambdaLift' :: [FunDefAnfT ByteString]
            -> [FunDefAnfT ByteString]
lambdaLift' funDefs =
    let (funDefs', ls) = runState (mapM (lambdaLiftDefn genName) funDefs) (LiftState 0 [])
    in
    lifted ls ++ funDefs'

alphaExpr = undefined

lambdaLiftDefn :: (Ord s, Show s, IsString s) => (s -> State (LiftState s) s)
                                              -> FunDefAnfT s
                                              -> State (LiftState s) (FunDefAnfT s)
lambdaLiftDefn nameGen (FunDefAnfT t n (AExp fun)) =

    case fun of
        ALam vs body -> do
            body' <- ll body
            pure $ FunDefAnfT undefined n (AExp (ALam vs body'))

        _ ->
            error "lambda-lifting non-lambda!"

    where
    --ll :: NExp s -> State (LiftState s) (NExp s)
    ll expr =
        case expr of

            AExp aexp ->
                AExp <$> lla aexp

            CExp cexp ->
                CExp <$> llc cexp

            NLet a (AExp b@ALam{}) c -> do
                b' <- liftLambdaOrClosure (Just a) b
                c' <- ll c
                pure $ NLet a (AExp b') c'

            NLet a (AExp b@AClo{}) c -> do
                b' <- liftLambdaOrClosure (Just a) b
                c' <- ll c
                pure $ NLet a (AExp b') c'

            NLet a b c ->
                NLet a <$> ll b
                       <*> ll c

    --lla :: AExp s -> State (LiftState s) (AExp s)
    lla aexp =

        case aexp of

            ATerm{} ->
                pure aexp

            ALam{} ->
                liftLambdaOrClosure Nothing aexp

            AClo{} ->
                liftLambdaOrClosure Nothing aexp

            AUnPrimOp o a ->
                AUnPrimOp o <$> lla a

            ABinPrimOp o a b ->
                ABinPrimOp o <$> lla a
                             <*> lla b

    --llc :: CExp s -> State (LiftState s) (CExp s)
    llc cexp =

        case cexp of

            -- TODO: Extract any CallClo arguments out as lets, prefixed as 'cc_'
            -- (informally keeping it in CPS style)
            CApp f xs -> do
                f'  <- lla f
                xs' <- mapM lla xs
                b <- extractLets f' [] [] xs'
                _ b

            CIfThenElse pr tr fl ->
                CIfThenElse <$> lla pr
                            <*> ll tr
                            <*> ll fl

    -- Handles let-bound and anonymous lambdas/closures
    -- renaming recursive calls if necessary
    liftLambdaOrClosure mName (ALam vs body) = do
        newName <- nameGen "llam"
        let body' =
                case mName of
                    -- If this lambda has a name
                    -- rename (recursive) references to it during the lift
                    Just oldName ->
                        let subst = foldr M.delete (M.singleton oldName newName) vs
                        in alphaExpr subst body
                    Nothing -> body
        lam' <- ALam vs <$> ll body'
        liftLambda $ FunDefAnfT newName undefined (AExp lam')
        pure . ATerm $ Var newName

    liftLambdaOrClosure mName (AClo fvs vs body) = do
        newName <- nameGen "lclo"
        let body' =
                case mName of
                    -- If this lambda has a name
                    -- rename (recursive) references to it during the lift
                    Just oldName ->
                        let subst = foldr M.delete (M.singleton oldName newName) (fvs++vs)
                        in alphaExpr subst body
                    Nothing -> body
        clo' <- AClo fvs vs <$> ll body'
        liftLambda $ FunDefAnfT newName undefined (AExp clo')
        undefined -- pure $ CallClo newName fvs

    liftLambdaOrClosure _ _ =
        error "Tried to lift non-lambda/closure"

    -- TODO test
    extractLets f lets args es =

        case es of

            [] ->
                pure $ foldr (\(a,b) c -> NLet a b c) (CExp (CApp f (reverse args))) (reverse lets)

            {-
            (cc@CallClo{}:xs) -> do
                name <- nameGen "cc"
                extractLets f ((name, cc) : lets) (ETerm (Var name) : args) xs
            -}

            (x:xs) ->
                extractLets f lets (x : args) xs

genName :: ByteString
        -> State (LiftState ByteString) ByteString
genName pref = do
    ls <- get
    let n = liftedNum ls
    put $! ls { liftedNum = n + 1}
    pure $ pref <> "_" <> pack (show n)

liftLambda :: FunDefAnfT s
           -> State (LiftState s) ()
liftLambda funDef = modify' $ \ls -> ls { lifted = funDef : lifted ls }
