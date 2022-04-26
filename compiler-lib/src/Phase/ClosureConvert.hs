module Phase.ClosureConvert (closureConvert) where

import Common.State
import Core.Definition
import Core.Expression
import Core.Term
import FreeVars.FreeVars

import           Control.Monad         (forM)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Maybe            (catMaybes)
import           Data.Set              (Set)
import qualified Data.Set as S

closureConvert :: Module ByteString -> Module ByteString
closureConvert md = md { getFunDefns = closureConvert' $ getFunDefns md }

closureConvert' :: [FunDefn ByteString] -> [FunDefn ByteString]
closureConvert' funDefs = do

    let topLevelScope = S.fromList $ map (\(FunDefn n _) -> n) funDefs

    let (exprs, ls) = runState (mapM (closureConvertDefn topLevelScope) funDefs) (LiftState mempty 0)

    liftedLambdas ls ++ exprs

data LiftState s =
    LiftState { liftedLambdas :: ![FunDefn s]
              , lambdaNum     :: !Int
              } deriving Show

closureConvertDefn :: Set ByteString
                   -> FunDefn ByteString
                   -> State (LiftState ByteString) (FunDefn ByteString)
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
                (liftedName, fvs) <- liftedLambda vs body'
                pure $
                    case fvs of
                        [] -> ETerm (Var liftedName)
                        _  -> MkClos liftedName fvs

            -- TODO: extract any MkClos in these arguments out into Lets
            -- TODO: When to replace EApp with CallClosure?
            EApp f xs ->
                liftClosureArgs (f:xs)

            ELet a b c ->
                ELet a <$> cc b
                       <*> cc c

            EUnPrimOp o a ->
                EUnPrimOp o <$> cc a

            EBinPrimOp o a b ->
                EBinPrimOp o <$> cc a
                             <*> cc b

            IfThenElse p t f ->
                IfThenElse <$> cc p
                           <*> cc t
                           <*> cc f

            EClos{} ->
                error "EClos does not exist yet."

            MkClos{} ->
                error "MkClos does not exist yet."

    liftedLambda vs body = do
        name <- naming "lifted_"
        alreadyLifted <- S.fromList . map (\(FunDefn na _) -> na) . liftedLambdas <$> get
        let freeVars = getFreeVars' (alreadyLifted <> topLevelScope) (ELam vs body)
        let lifted = if null freeVars
                        then ELam           vs body
                        else EClos freeVars vs body
        modify' $ \ls -> ls { liftedLambdas = (FunDefn name lifted) : liftedLambdas ls }
        pure (name, freeVars)

    liftClosureArgs :: [Expr ByteString]
                    -> State (LiftState ByteString) (Expr ByteString)

    liftClosureArgs fargs = do

        pairs <- forM fargs $ \x -> do
                     x' <- cc x
                     case x' of
                         ETerm{}  -> pure (Nothing, x')
                         MkClos{} -> do
                             clo <- naming "clo_"
                             pure (Just (clo, x'), ETerm (Var clo))
                         _ -> error "Non term/mkclos as parameter!"

        let (lets, f:args) = unzip pairs

        -- Is this the right order?
        pure $ foldr (\(l,c) -> ELet l c)
                     (EApp f args)
                     (catMaybes lets)

naming :: String -> State (LiftState ByteString) ByteString
naming prefix = State $ \ls -> (pack (prefix <> show (lambdaNum ls)), ls { lambdaNum = lambdaNum ls + 1})