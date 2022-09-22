{-# LANGUAGE OverloadedStrings #-}

module Phase.LambdaLift.LambdaLift (lambdaLift) where

import Common.State
import Core.Module
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule
import Phase.LambdaLift.Alpha

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.String           (IsString)
import qualified Data.Map as M

data LiftState s =
    LiftState { getLiftedNum :: !Int
              , getLifted    :: ![FunDefAnfT s]
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
    getLifted ls ++ funDefs'

lambdaLiftDefn :: (Ord s, Show s, IsString s) => (s -> State (LiftState s) s)
                                              -> FunDefAnfT s
                                              -> State (LiftState s) (FunDefAnfT s)
lambdaLiftDefn nameGen fd@(FunDefAnfT t n fun) =

    case fun of

        -- Top-level lambdas do not need to be lifted!
        AExp (ALam vs body) ->
            FunDefAnfT t n . AExp . ALam vs <$> ll body

        -- Everything else gets lifted
        _ -> pure fd

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

            NLet _ (AExp AClo{}) _ ->
                error "Lift a closure not implemented"

            NLet a b c ->
                NLet a <$> ll b
                       <*> ll c

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

    llc cexp =

        case cexp of

            CApp f xs ->
                CApp <$> lla f
                     <*> mapM lla xs

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
                        in alphaNExp subst body
                    Nothing -> body
        lam' <- AExp . ALam vs <$> ll body'
        let q = generalise lam'
        liftLambda $ FunDefAnfT newName q lam'
        pure . ATerm $ Var newName

    -- TODO dedupe with above
    liftLambdaOrClosure mName (AClo fvs vs body) = do
        newName <- nameGen "lclo"
        let body' =
                case mName of
                    -- If this lambda has a name
                    -- rename (recursive) references to it during the lift
                    Just oldName ->
                        let subst = foldr M.delete (M.singleton oldName newName) vs
                        in alphaNExp subst body
                    Nothing -> body
        lam' <- AExp . AClo fvs vs <$> ll body'
        let q = generalise lam'
        liftLambda $ FunDefAnfT newName q lam'
        pure . ATerm $ Var newName

-- TODO: Actually calculate the free type variables
generalise :: NExp s -> Quant s
generalise _ = Quant []

genName :: ByteString
        -> State (LiftState ByteString) ByteString
genName pref = do
    ls <- get
    let n = getLiftedNum ls
    put $! ls { getLiftedNum = n + 1}
    pure $ pref <> "_" <> pack (show n)

liftLambda :: FunDefAnfT s
           -> State (LiftState s) ()
liftLambda funDef = modify' $ \ls -> ls { getLifted = funDef : getLifted ls }
