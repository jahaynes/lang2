{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Runtimes.CeskMachine (runMachine) where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))

import           Control.Monad   (foldM)
import           Data.ByteString (ByteString)
import           Data.Functor    ((<&>))
import           Data.Map.Strict ((!), Map)
import qualified Data.Map as M

{-
    TODO:
        * Make sure returns are happening (and items are leaving env scope)
        * Add a Stack to go along with a heap?
        * Pre-compile / partially evaluate?
        * Ints as Addr - instead of heap?
        * Use recursion/planning to trim down env
        * Do lambda lifting!
-}

newtype Addr =
    Addr Int
        deriving (Eq, Ord, Show)

data Val s = VBool !Bool
           | VInt !Integer
           | VString !s
           | VClo ![s] !(NExp s) !(Env s)
               deriving Show

data Cesk s =
    Cesk { getCode :: !(NExp s)
         , getEnv  :: !(Env s)
         } deriving Show

newtype Env s =
    Env (Map s Addr)
        deriving Show

newtype Store s =
    Store (Map Addr (Val s))
        deriving Show

data Heap s =
    Heap { getStore :: !(Store s)
         , getFree  :: !Addr
         } deriving Show

instance Ord s => Semigroup (Env s) where
    Env e1 <> Env e2 = Env (e1 <> e2)

instance Ord s => Monoid (Env s) where
    mempty = Env mempty

heap0 :: Heap s
heap0 = Heap (Store mempty) (Addr 0)

runMachine :: AnfModule ByteString -> IO ()
runMachine modu = do

    let topLevels = map (\(FunDefAnfT n _ e) -> (n, e)) $ getFunDefAnfTs modu

    let a = evalState' heap0 $ do
                env             <- bindTopLevels topLevels
                VClo [] start _ <- lkup env "main"
                evalExpr env start

    print a

lkup :: (Ord k, Show k) => Env k -> k -> State (Heap s) (Val s)
lkup (Env e) n = do
    Heap (Store s) _ <- get
    case M.lookup n e of
        Nothing -> error $ "Not found in env: " <> show n
        Just addr ->
            case M.lookup addr s of
                Nothing -> error "Not found in store"
                Just v -> pure v

evalExpr :: (Ord s, Semigroup s, Show s) => Env s -> NExp s -> State (Heap s) (Val s)
evalExpr env (AExp aexp) = evalAexp env aexp
evalExpr env (CExp cexp) = evalCexp env cexp
evalExpr env@(Env e) (NLet a b c) = do
    bAddr <- allocVal =<< evalExpr env b
    let localEnv = Env $ M.insert a bAddr e
    evalExpr localEnv c

evalAexp :: (Ord s, Semigroup s, Show s) => Env s -> AExp s -> State (Heap s) (Val s)
evalAexp env aexp =

    case aexp of

        ATerm t -> evalTerm env t

        {-
        TODO  To/From string typeclass?
        AUnPrimOp EShow a -> do
            a' <- evalAexp env a
            pure . VString $ show a'
        -}

        ABinPrimOp op a b ->
            bothM (evalAexp env) (a, b) <&> \(a', b') ->
            case op of
                AddI    -> arith (+) a' b'
                SubI    -> arith (-) a' b'
                MulI    -> arith (*) a' b'
                DivI    -> arith div a' b'
                ModI    -> arith mod a' b'
                LtEqI   -> comparison (<=) a' b'
                LtI     -> comparison (<)  a' b'
                GtEqI   -> comparison (>=) a' b'
                GtI     -> comparison (>)  a' b'
                EqA     -> eqA  a' b'
                AndB    -> logical (&&) a' b'
                OrB     -> logical (||) a' b'
                ConcatS -> stringy (<>) a' b'

        ALam vs body ->
            pure $ VClo vs body env

        AClo fvs vs body ->
            let Env e = env
                cloEnv = Env . M.fromList $ map (\fv -> (fv, e ! fv)) fvs
            in pure $ VClo vs body cloEnv

stringy :: (s -> s -> s) -> Val s -> Val s -> Val s
stringy f (VString a) (VString b) = VString $ f a b
stringy _ _ _                     = error "Expected stringlike"

arith :: (Integer -> Integer -> Integer) -> Val s -> Val s -> Val s
arith f (VInt a) (VInt b) = VInt $ f a b
arith _ _ _               = error "Expected integers"

logical :: (Bool -> Bool -> Bool) -> Val s -> Val s -> Val s
logical f (VBool a) (VBool b) = VBool $ f a b
logical _ _ _                 = error "Expected booleans"

comparison :: (Integer -> Integer -> Bool) -> Val s -> Val s -> Val s
comparison f (VInt a) (VInt b) = VBool $ f a b
comparison _ _ _               = error "Expected integers"

eqA :: Eq a => Val a -> Val a -> Val s
eqA    (VInt a)    (VInt b) = VBool $ a == b
eqA   (VBool a)   (VBool b) = VBool $ a == b
eqA (VString a) (VString b) = VBool $ a == b
eqA VClo{} VClo{}           = error "comparing closures"
eqA _ _                     = error "Mistyped equality"

evalTerm :: (Ord s, Semigroup s, Show s) => Env s -> Term s -> State (Heap s) (Val s)
evalTerm env term =
    case term of
        LitBool b   -> pure $ VBool b
        LitInt i    -> pure $ VInt i
        LitString s -> pure $ VString s
        Var v       ->
            -- This evaluates on the lookup, maybe do it on insert instead?
            lkup env v >>= \case
                VClo [] body _ -> evalExpr env body
                val            -> pure val
        DCons{}     -> error "not implemented"

evalCexp :: (Ord s, Semigroup s, Show s) => Env s -> CExp s -> State (Heap s) (Val s)
evalCexp env cexp =

    case cexp of

        CApp f xs -> do
            xs'  <- mapM (\x -> allocVal =<< evalAexp env x) xs
            VClo vs body cloEnv <- evalAexp env f
            let vsMap = Env . M.fromList $ zip vs xs'
            let new = vsMap <> cloEnv <> env -- earlier has precedence
            evalExpr new body

        CIfThenElse pr tr fl ->
            evalAexp env pr >>= \case
                VBool b ->
                    if b
                        then evalExpr env tr
                        else evalExpr env fl
                _ -> error "Mistyped predicate"

bindTopLevels :: (Ord s, Show s) => [(s, NExp s)]
                                 -> State (Heap s) (Env s)
bindTopLevels = foldM step (Env mempty)
    where
    step (Env env) (n, e) = do
        addr <- allocVal (topLevelToVal e)
        pure . Env $ M.insert n addr env

    topLevelToVal :: Ord s => NExp s -> Val s
    topLevelToVal expr =
        case expr of
            AExp (ALam vs body) -> VClo vs body (Env mempty)
            _ -> VClo [] expr (Env mempty)

allocVal :: (Show s) => Val s -> State (Heap s) Addr
allocVal v = do
    Heap (Store s) addr <- get
    let s' = M.insert addr v s
    put $ Heap (Store s') (next addr)
    pure addr

next :: Addr -> Addr
next (Addr a) = Addr (a+1)

bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = do
    fx <- f x
    fy <- f y
    pure (fx, fy)
