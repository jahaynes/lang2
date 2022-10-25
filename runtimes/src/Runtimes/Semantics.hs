{-# LANGUAGE OverloadedStrings #-}

module Runtimes.Semantics (runMachine) where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))

import           Control.Monad         (foldM)
import           Data.ByteString.Char8 (ByteString, pack, unpack)
--import           Data.Functor          ((<&>))
import           Data.Map.Strict       ((!), Map)
import qualified Data.Map as M
-- import           Data.Sequence         (Seq (..), (|>), empty)
--import           Text.Printf           (printf)


newtype StackAddr =
    StackAddr Int
        deriving (Eq, Ord, Show)

newtype HeapAddr =
    HeapAddr Int
        deriving (Eq, Ord, Show)

newtype Stack a =
    Stack [a]
        deriving Show

newtype Env =
    Env (Map ByteString HeapAddr)
        deriving Show

newtype Heap =
    Heap (Map HeapAddr Val)
        deriving Show

data Val = VClo
         | VLam Val
         | VBinOp BinOp Val Val
         | VStackValAt Int
         | VHeapValAt Int
         | VUnknown
         | VApp Val [Val]
         | VInt Integer
             deriving Show


runMachine :: AnfModule ByteString -> IO (ByteString)
runMachine modu = do

    let env0  = Env mempty
    let heap0 = Heap mempty

    -- Preflight
    loadedModu <- loadModule (env0, HeapAddr 0, heap0, modu)

    -- must not use Env (or anything else preflight) onwards from here....

    error "TODO"

{- START Preflight -}

loadModule :: (Env, HeapAddr, Heap, AnfModule ByteString) -> IO ()
loadModule (env, freeHeap, heap, modu) = do

    let funDefs = getFunDefAnfTs modu

    (env',  _        ) <- foldM loadFunctionName        (env,  freeHeap) funDefs
    (heap', freeHeap') <- foldM (loadFunctionBody env') (heap, freeHeap) funDefs

    print env'
    print heap'

loadFunctionName :: (Env, HeapAddr) -> FunDefAnfT ByteString -> IO (Env, HeapAddr)
loadFunctionName (Env env, heapAddr) (FunDefAnfT name _ _) = do
    print name
    let env' = Env $ M.insert name heapAddr env
    pure (env', next heapAddr)
    
loadFunctionBody :: Env -> (Heap, HeapAddr) -> FunDefAnfT ByteString -> IO (Heap, HeapAddr)
loadFunctionBody env (Heap heap, heapAddr) (FunDefAnfT _ _ body) = do
    body' <- asVal env mempty body
    let heap' = Heap $ M.insert heapAddr body' heap
    pure (heap', next heapAddr)

lkupStackAddr :: ByteString -> Map ByteString Int -> IO Val
lkupStackAddr a stackAddrs =
    case M.lookup a stackAddrs of
        Nothing  -> error $ "missing from stack: " ++ show a
        Just i   -> pure $ VStackValAt i

lkupStackOrHeap :: ByteString -> Env -> Map ByteString Int -> IO Val
lkupStackOrHeap a (Env env) stackAddrs =
    case M.lookup a stackAddrs of
        Just i  -> pure $ VStackValAt i
        Nothing ->
            case M.lookup a env of
                Just (HeapAddr i)  -> pure $ VHeapValAt i
                Nothing -> error $ "Not in stack or heap: " ++ show a
        

asVal :: Env -> Map ByteString Int -> NExp ByteString -> IO Val
asVal env stackAddrs (AExp aexp) = asValA env stackAddrs aexp
asVal env stackAddrs (CExp cexp) = asValC env stackAddrs cexp

asValA :: Env -> Map ByteString Int -> AExp ByteString -> IO Val
asValA env stackAddrs aexp =

    case aexp of

        ATerm _ (Var v) ->
            lkupStackOrHeap v env stackAddrs

        ATerm _ (LitInt i) ->
            pure $ VInt i


        ALam _ vs body ->
            -- Convert vs to stack addrs
            let stackAddrs' = M.fromList $ zip vs [0..]
            in VLam <$> asVal env stackAddrs' body

        ABinPrimOp _ op a b -> do
            a' <- asValA env stackAddrs a
            b' <- asValA env stackAddrs b
            pure $ VBinOp op a' b'

        _ -> error $ show aexp

asValC :: Env -> Map ByteString Int -> CExp ByteString -> IO Val
asValC env stackAddrs cexp =
    case cexp of
        CIfThenElse _ _pr _tr _fl -> error "if cexp"
        CApp        _ f xs        -> do
            f'  <-       asValA env stackAddrs f
            xs' <- mapM (asValA env stackAddrs) xs
            pure $ VApp f' xs'
        CCase       _ _scrut ps   -> error "caseof cexp"

{- END Preflight -}






{-
    prepareFunction (modu, "main", empty, empty)




prepareFunction (modu, funName, evaldArgs, unevaldArgs) =
    case unevaldArgs of
        Empty ->
            let funAddr = lookupFun funName
            in callFunction (modu, funAddr, evaldArgs)
        ua :<| uas ->
            let a = force ua
            in prepareFunction (modu, funName, evaldArgs |> a, uas)

callFunction (modu, funAddr, evaldArgs) =
    pure "foo"

force x = x

lookupFun y = y
-}






class Next a where
    next :: a -> a

instance Next StackAddr where
    next (StackAddr i) = StackAddr $ i + 1

instance Next HeapAddr where
    next (HeapAddr i) = HeapAddr $ i + 1