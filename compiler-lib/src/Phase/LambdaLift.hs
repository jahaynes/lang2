{-# LANGUAGE OverloadedStrings #-}

module Phase.LambdaLift (lambdaLift) where

import Common.State
import Core.Definition
import Core.Expression
import Core.Term
import Optimise.Alpha

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.String           (IsString)
import qualified Data.Map as M

data LiftState s =
    LiftState { liftedNum    :: !Int
              , lifted       :: ![FunDefn s]
              } deriving Show

lambdaLift :: Module ByteString -> Module ByteString
lambdaLift md =
    md { getFunDefns = lambdaLift' $ getFunDefns md }

lambdaLift' :: [FunDefn ByteString] -> [FunDefn ByteString]
lambdaLift' funDefs =
    let (funDefs', ls) = runState (mapM (lambdaLiftDefn genName) funDefs) (LiftState 0 [])
    in
    lifted ls ++ funDefs'

lambdaLiftDefn :: (Ord s, Show s, IsString s) => (s -> State (LiftState s) s)
                             -> FunDefn s
                             -> State (LiftState s) (FunDefn s)
lambdaLiftDefn nameGen (FunDefn n fun) =

    case fun of
        ELam vs body -> FunDefn n . ELam vs <$> ll body
        _            -> error "lambda-lifting non-lambda!"

    where
    ll e =
        case e of

            ETerm{} ->
                pure e

            ELam{} ->
                liftLambdaOrClosure Nothing e

            EClo{} ->
                liftLambdaOrClosure Nothing e

            -- TODO: Extract any CallClo arguments out as lets, prefixed as 'cc_'
            -- (informally keeping it in CPS style)
            EApp f xs -> do
                f'  <- ll f
                xs' <- mapM ll xs
                extractLets f' [] [] xs'

            ELet a b@ELam{} c -> do
                b' <- liftLambdaOrClosure (Just a) b
                c' <- ll c
                pure $ ELet a b' c'

            ELet a b@EClo{} c -> do
                b' <- liftLambdaOrClosure (Just a) b
                c' <- ll c
                pure $ ELet a b' c'

            ELet a b c ->
                ELet a <$> ll b
                       <*> ll c

            EUnPrimOp o a ->
                EUnPrimOp o <$> ll a

            EBinPrimOp o a b ->
                EBinPrimOp o <$> ll a
                             <*> ll b

            IfThenElse p t f ->
                IfThenElse <$> ll p
                           <*> ll t
                           <*> ll f

            CallClo{} ->
                error "Doesn't exist yet!"

    -- Handles let-bound and anonymous lambdas/closures
    -- renaming recursive calls if necessary
    liftLambdaOrClosure mName (ELam vs body) = do
        newName <- nameGen "llam"
        let body' =
                case mName of
                    -- If this lambda has a name
                    -- rename (recursive) references to it during the lift
                    Just oldName ->
                        let subst = foldr M.delete (M.singleton oldName newName) vs
                        in alphaExpr subst body
                    Nothing -> body
        lam' <- ELam vs <$> ll body'
        liftLambda $ FunDefn newName lam'
        pure . ETerm $ Var newName

    liftLambdaOrClosure mName (EClo fvs vs body) = do
        newName <- nameGen "lclo"
        let body' =
                case mName of
                    -- If this lambda has a name
                    -- rename (recursive) references to it during the lift
                    Just oldName ->
                        let subst = foldr M.delete (M.singleton oldName newName) (fvs++vs)
                        in alphaExpr subst body
                    Nothing -> body
        clo' <- EClo fvs vs <$> ll body'
        liftLambda $ FunDefn newName clo'
        pure $ CallClo newName fvs

    liftLambdaOrClosure _ _ =
        error "Tried to lift non-lambda/closure"

    -- TODO test
    extractLets f lets args es =

        case es of

            [] ->
                pure $ foldr (\(a,b) c -> ELet a b c) (EApp f (reverse args)) (reverse lets)

            (cc@CallClo{}:xs) -> do
                name <- nameGen "cc"
                extractLets f ((name, cc) : lets) (ETerm (Var name) : args) xs

            (x:xs) ->
                extractLets f lets (x : args) xs

genName :: ByteString -> State (LiftState ByteString) ByteString
genName pref = do
    ls <- get
    let n = liftedNum ls
    put $! ls { liftedNum = n + 1}
    pure $ pref <> "_" <> pack (show n)

liftLambda :: FunDefn s -> State (LiftState s) ()
liftLambda funDef = modify' $ \ls -> ls { lifted = funDef : lifted ls }
