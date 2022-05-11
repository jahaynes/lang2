{-# LANGUAGE OverloadedStrings #-}

module AbstractMachine.Common where

import Common.EitherT
import Common.State
import Core.Expression
import Core.Term
import Core.Operator

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.IntMap           (IntMap, (!))
import qualified Data.IntMap as IM
import           Data.Map              (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace (trace)

data Heap s =
    Heap { nextFree :: !Int
         , storage  :: !(IntMap (CloEnv s))
         }

newtype CloEnv s =
    CloEnv (Map s (Expr s)) -- TODO make sure all of these are denotable
        deriving Show

empty :: Heap a
empty = Heap 0 mempty

eval :: Map ByteString (Expr ByteString)
     -> Expr ByteString
     -> Either ByteString (Expr ByteString)
eval env e = do
    fst $ runState (runEitherT (go env e)) empty

lift :: Functor m => m a -> EitherT ByteString m a
lift = EitherT . fmap Right

lookupEnv env v =
    case M.lookup v env of
        Nothing -> left "var not found in env!"
        Just x  -> pure x

storeHeap :: CloEnv s -> State (Heap s) Int
storeHeap cloEnv = do
    heap <- get
    let ptr = nextFree heap
    put $ heap { nextFree = ptr + 1
               , storage  = IM.insert ptr cloEnv (storage heap) }
    
    trace ("+Heap : " <> show ptr) $
    
        pure ptr

readHeap :: Int -> EitherT ByteString (State (Heap s)) (CloEnv s)
readHeap ptr = trace ("reading from heap " <> show ptr) $ lift $ do
    heap <- get
    pure (storage heap ! ptr)

go :: (Show s, Ord s) => Map s (Expr s)
                      -> Expr s
                      -> EitherT ByteString (State (Heap s)) (Expr s)

go env e@(ETerm t) =
    case t of
        Var v -> lookupEnv env v
        _     -> pure e

go _ e@ELam{} =
    pure e

go env (InstantiateClos fp fvs) = do
    let fvs' = S.fromList fvs
    let cloEnv = CloEnv $ M.filterWithKey (\k _ -> S.member k fvs') env
    ptr <- lift $ storeHeap cloEnv
    pure $ Instance fp ptr  -- TODO not needed fp?

go env (EApp f xs) = do

    fxs' <- mapM (go env) (f:xs)

    case fxs' of

        ELam vs e:xs' -> do
            zipped <- zipe (pack $ show f) vs xs'
            let env' = foldr (uncurry M.insert) env zipped
            go env' e
            

go env (ELet a b c) = do
    b' <- go env b
    go (M.insert a b' env) c

go env u@(EUnPrimOp o a) = do
    a' <- go env a
    case (o, a') of
        (Negate, ETerm (LitInt i)) -> pure $ ETerm (LitInt (-i))
        _ -> left $ "Unknown unop: " <> pack (show u)

go env i@(IfThenElse p t f) = do
    p' <- go env p
    case p' of
        ETerm (LitBool True)  -> go env t
        ETerm (LitBool False) -> go env f
        _                     -> left $ "Bad ifthenelse: " <> pack (show i)

go env (EBinPrimOp op a b) = do
    ab' <- mapM (go env) [a, b]
    case (op, ab') of

        (AddI, [ETerm (LitInt a''), ETerm (LitInt b'')]) ->
            pure $ ETerm (LitInt (a'' + b''))

        (SubI, [ETerm (LitInt a''), ETerm (LitInt b'')]) ->
            pure $ ETerm (LitInt (a'' - b''))

        (EqA, [a', b']) ->
            pure $ ETerm (LitBool (a' == b'))

        (OrB, [ETerm (LitBool a''), ETerm (LitBool b'')]) ->
            pure $ ETerm (LitBool (a'' || b''))

        _ -> left $ "Unknown primop combo: " <> pack (show (op, ab'))



go e x =
    left $ pack ("Unknown: " ++ show (x, e))

zipe :: (Show a, Show b) => Applicative m => ByteString -> [a] -> [b] -> EitherT ByteString m [(a, b)]
zipe dbgname xs ys
    | length xs /= length ys = left $ "Mismatch in arg length for: " <> dbgname <> (pack $ show (xs, ys))
    | otherwise              = pure $ zip xs ys
