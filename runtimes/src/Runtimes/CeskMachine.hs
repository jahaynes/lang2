{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Runtimes.CeskMachine (runMachine) where

import Common.CallGraph
import Common.Writer
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))

import           Control.Monad   (foldM, unless)
import           Data.ByteString (ByteString)
import           Data.Functor    ((<&>))
import           Data.List       (foldl')
import           Data.Map.Strict ((!), Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Printf     (printf)

newtype Addr =
    Addr Int
        deriving (Eq, Ord, Show)

data Val s = VBool !Bool
           | VInt !Integer
           | VString !s
           | VClo ![s] !(NExp s) !(Env s)
               deriving Show

data Kont s = KHalt
            | KLet !s !(NExp s) !(Env s) !(Kont s)
                deriving Show

data Cesk s =
    Cesk { getCode  :: !(NExp s)
         , getEnv   :: !(Env s)
         , getStore :: !(Store s)
         , getKont  :: !(Kont s)
         , getFree  :: !Addr
         } deriving Show

newtype Env s =
    Env (Map s Addr)
        deriving Show

newtype Store s =
    Store (Map Addr (Val s))
        deriving Show

runMachine :: AnfModule ByteString -> IO ()
runMachine modu = do

        -- Figure out the order in which to load the top-levels
    let Right plan   = map S.toList <$> createPlan (buildGraphAnf modu)
        topLevelsMap = M.fromList . map (\(FunDefAnfT n _ e) -> (n, e)) $ getFunDefAnfTs modu

        -- Prep the machine
        initialMachine = (Env mempty, Store mempty, Addr 0)

    prepped <- foldM (prepTopLevelGroup topLevelsMap)
                     initialMachine
                     plan

    printMachine prepped

prepTopLevelGroup :: (Ord s, Show s) => Map s (NExp s)
                                     -> (Env s, Store s, Addr)
                                     -> [s]
                                     -> IO (Env s, Store s, Addr)
prepTopLevelGroup topLevelsMap machine@(env, store, addr) group = do

    let namedExprs = map (\g -> (g, topLevelsMap ! g)) group

        ((env', store', addr'), logs) =
                runWriter [] (bindEvalMutRecursive env store addr namedExprs)

    unless (null logs) $ do
            putStrLn $ show namedExprs ++ " logs:"
            mapM_ print logs
            putStrLn ""

    pure (env', store', addr')

bindEvalMutRecursive :: (Ord s, Show s) => Env s
                                        -> Store s
                                        -> Addr
                                        -> [(s, NExp s)]
                                        -> Writer String (Env s, Store s, Addr)
bindEvalMutRecursive env store addr namedExprs = do

    let addrs =
            take (length namedExprs) $ iterate next addr

        namedAddresses =
            zipWith (\(g, _) a -> (g, a)) namedExprs addrs

        env' =
            foldl' prepEnv env namedAddresses

        addressedExprs =
            zipWith (\(_, e) a -> (a, e)) namedExprs addrs

        addr' =
            next $ last addrs

    foldM prepStore (env', store, addr') addressedExprs

    where
    prepEnv (Env e) (g, a) =
        Env $ M.insert g a e

    prepStore (env', store', addr') (a, ex) = do
        (val, addr'') <- evalExpr env' store' addr' ex
        store''       <- insertStore a val store'
        pure (env', store'', addr'')

printMachine :: Show s => (Env s, Store s, Addr) -> IO ()
printMachine (Env env, Store store, _) = do
    putStrLn "Env:"
    mapM_ print $ M.toList env
    putStrLn "\nStore:"
    mapM_ print $ M.toList store

insertEnv :: (Ord s, Show s) => s -> Addr -> Env s -> Writer String (Env s)
insertEnv v a (Env env) = do
    write $ printf "Binding %s at %s" (show v) (show a)
    pure . Env $ M.insert v a env

insertStore :: Addr -> Val s -> Store s -> Writer String (Store s)
insertStore k v (Store store) =
    pure . Store $ M.insert k v store

next :: Addr -> Addr
next (Addr a) = Addr (a + 1)

evalExpr :: (Ord s, Show s) => Env s
                            -> Store s
                            -> Addr
                            -> NExp s
                            -> Writer String (Val s, Addr)
evalExpr env store addr expr =

    case expr of

        AExp aexp -> do
            val <- evalAtom env store aexp
            pure (val, addr)

        CExp cexp ->
            evalCexp env store addr cexp

        NLet v bexpr body -> do
            (val, addr') <- evalExpr env store addr bexpr
            env'         <- insertEnv v addr' env
            store'       <- insertStore addr' val store
            let addr'' = next addr'
            evalExpr env' store' addr'' body -- is this a Kont?

evalCexp :: (Ord s, Show s) => Env s
                            -> Store s
                            -> Addr
                            -> CExp s
                            -> Writer String (Val s, Addr)
evalCexp env store addr cexp =

    case cexp of

        CApp f xs -> do
            f'  <-       evalAtom env store  f
            xs' <- mapM (evalAtom env store) xs
            write $ "Calling " ++ show f
            (e', env', s', a') <- applyProc store addr f' xs'
            evalExpr env' s' a' e'

        CIfThenElse pr tr fl -> do
            pr' <- evalBool env store pr
            if pr'
                then evalExpr env store addr tr
                else evalExpr env store addr fl

evalAtom :: (Ord s, Show s) => Env s
                            -> Store s
                            -> AExp s
                            -> Writer String (Val s)
evalAtom env store expr =

    case expr of

        ATerm term -> do
            write (printf "Evaling term %s in env %s and store %s" (show term) (show env) (show store))
            evalTerm env store term

        ALam vs body ->
            pure $ VClo vs body (Env mempty) -- env -- shouldn't have env here?

        AClo fvs vs body -> do
            let Env e   = env
                closEnv = Env . M.fromList $ map (\v -> (v, e ! v)) fvs
            pure $ VClo vs body closEnv

        ABinPrimOp op a b ->

            case op of

                AddI -> evalAdd <$> evalInt env store a
                                <*> evalInt env store b

                SubI -> evalSub <$> evalInt env store a
                                <*> evalInt env store b

                EqA -> evalEq <$> evalAtom env store a
                              <*> evalAtom env store b

    where
    evalAdd a b = VInt $ a + b
    evalSub a b = VInt $ a - b
    evalEq (VBool a) (VBool b) = VBool $ a == b
    evalEq (VInt a) (VInt b) = VBool $ a == b
    evalEq (VString a) (VString b) = VBool $ a == b

evalTerm :: (Ord s, Show s) => Env s
                            -> Store s
                            -> Term s
                            -> Writer String (Val s)
evalTerm (Env env) (Store store) term =
    case term of
        DCons{}     -> error "Not implemented"
        LitBool b   -> pure $ VBool b
        LitInt i    -> pure $ VInt i
        LitString s -> pure $ VString s
        Var v       ->
            case M.lookup v env of
                Nothing   -> dumpErrorLog ("Bad v: " ++ show v)
                Just addr ->
                    case M.lookup addr store of
                        Nothing  -> dumpErrorLog ("Bad addr: " ++ show addr)
                        Just val -> pure val

dumpErrorLog :: String -> Writer String a
dumpErrorLog msg = do
    lg <- reverse <$> getLog
    error $ unlines (msg:"":lg)

evalInt :: (Ord s, Show s) => Env s
                           -> Store s
                           -> AExp s
                           -> Writer String Integer
evalInt env store a =
    evalAtom env store a <&> \case
        VInt a' -> a'
        _       -> error "Nope"

evalBool :: (Ord s, Show s) => Env s
                            -> Store s
                            -> AExp s
                            -> Writer String Bool
evalBool env store b =
    evalAtom env store b <&> \case
        VBool b' -> b'
        _        -> error "Nope"

applyProc :: (Ord s, Show s) => Store s
                             -> Addr
                             -> Val s
                             -> [Val s]
                             -> Writer String (NExp s, Env s, Store s, Addr)
applyProc store (Addr a) f xs =

    case f of
        VClo vs e p -> do
            let as = map Addr [a ..]
            p' <- foldM (\env (v, adr) -> insertEnv v adr env) p (zip vs as)
            s' <- foldM (\sto (adr, x) -> insertStore adr x sto) store (zip as xs)
            pure (e, p', s', Addr $! a + length vs)

        _ -> error "Expected a closure"
