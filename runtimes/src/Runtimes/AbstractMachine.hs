{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}

module Runtimes.AbstractMachine (runMachine) where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))

import           Control.Monad         (foldM)
import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Data.Functor          ((<&>))
import           Data.Map.Strict       ((!), Map)
import qualified Data.Map as M
import           Debug.Trace           (trace)
import           Text.Printf           (printf)

{-
    TODO:
        * Make sure returns are happening (and items are leaving env scope)
        * Pre-compile / partially evaluate?
        * Ints as Addr - instead of heap?
        * Use recursion/planning to trim down env
-}

newtype HeapAddr =
    HeapAddr Int
        deriving (Eq, Ord, Show)

newtype StackAddr =
    StackAddr Int
        deriving (Eq, Ord, Show)

newtype StaticAddr =
    StaticAddr Int
        deriving (Eq, Ord, Show)

-- merge into Val
data Ptr = StaticA !StaticAddr
         | HeapA !HeapAddr
         | StackA !StackAddr
         | StackInt !Integer
         | StackBool !Bool
              deriving (Eq, Ord, Show)

data Val s = VBool !Bool
           | VInt !Integer
           | VString !s                     -- make this a ptr type
           | VClo ![s] !(NExp s) !(Env s)   -- make this a ptr type
           | VDCons s
           | VDConsApp s [Ptr]

instance Show s => Show (Val s) where

    show (VBool b) = show b
    show (VInt i) = show i
    show (VString s) = show s
    show (VDCons d) = show d
    show (VDConsApp d ps) = printf "%s %s" (show d) (unwords $ map show ps)
    show (VClo vs body (Env cloEnv)) =
        let vs'     = unwords $ map show vs
            cloEnv' = show $ M.toList cloEnv
            body'   = show body
        in printf "(\\%s %s {%s})" vs' body' cloEnv'

newtype Env s =
    Env (Map s Ptr)
        deriving Show

newtype Heap s =
    Heap (Map HeapAddr (Val s))
        deriving Show

newtype Static s =
    Static (Map StaticAddr (Val s))
        deriving Show

data Machine s =
    Machine { getStatic     :: !(Static s)
            , getStaticFree :: !(StaticAddr)
            , getStack      :: !(Stack s)
            , getStackFree  :: !(StackAddr)
            , getHeap       :: !(Heap s)
            , getFree       :: !HeapAddr
            } deriving Show

newtype Stack s =
    Stack (Map StackAddr (Val s)) -- TODO make actual stack
        deriving Show

newtype SByteString =
    SByteString ByteString
        deriving (Eq, Ord, Semigroup)

instance Show SByteString where
    show (SByteString bs) = show bs

instance Ord s => Semigroup (Env s) where
    Env e1 <> Env e2 = Env (e1 <> e2)

instance Ord s => Monoid (Env s) where
    mempty = Env mempty

class FromString s where
    fromString :: String -> s

instance FromString SByteString where
    fromString = SByteString . pack

class (Ord s, Semigroup s, FromString s, Show s) => Stringish s where

instance Stringish SByteString

{-
    TODO:
        * Make sure returns are happening (and items are leaving env scope)
        * Add a Stack to go along with a heap?
        * Pre-compile / partially evaluate?
        * Ints as Addr - instead of heap?
        * Use recursion/planning to trim down env
        * Do lambda lifting!
-}

machine0 :: Machine s
machine0 = Machine (Static mempty) (StaticAddr 0)
                   (Stack mempty)  (StackAddr 0)
                   (Heap mempty)   (HeapAddr 0)

runMachine :: AnfModule ByteString -> ByteString
runMachine modu = do

    let topLevels = map (\(FunDefAnfT n _ e) -> (SByteString n, SByteString <$> e)) $ getFunDefAnfTs modu

    let a = evalState' machine0 $ do
                env     <- bindTopLevels topLevels
                envMain <- lkup env (SByteString "main")
                case envMain of
                    VClo [] start _ -> do
                        machineRender =<< evalExpr env start
                    _ -> error $ show envMain

    pack a

machineRender :: Val SByteString
              -> State (Machine SByteString) String
machineRender x =

    case x of

        VDCons (SByteString s) ->
            pure $ unpack s

        VDConsApp (SByteString s) ps -> do
            ps' <- mapM machineRender' ps
            pure $ printf "%s %s" (unpack s) (unwords ps')

        VInt i ->
            pure $ show i

        VBool b ->
            pure $ show b

        VString (SByteString s) ->
            pure . show $ unpack s

        VClo vs body cloEnv ->
            pure $ show ("CLO", vs, body, cloEnv)

machineRender' :: Ptr -> State (Machine SByteString) String
machineRender' x =

    case x of

        -- Probably shouldn't get stack vars here?
        StackInt n ->
            pure $ show n

        StaticA ptr -> do
            Static static <- getStatic <$> get
            let staticVal = static ! ptr
            machineRender staticVal

        HeapA ptr -> do
            Heap heap <- getHeap <$> get
            let heapVal = heap ! ptr
            machineRender heapVal

        _ -> error $ show x

lkup :: (Ord k, Show k) => Env k -> k -> State (Machine s) (Val s)
lkup (Env e) n = do
    Machine (Static static) _ (Stack stack) _ (Heap heap) _ <- get
    case M.lookup n e of

        Nothing ->
            error $ "Not found in env: " <> show n

        Just (StaticA addr) ->
            case M.lookup addr static of
                Nothing -> error "Not found in static"
                Just v  -> pure v

        Just (HeapA addr) ->
            case M.lookup addr heap of
                Nothing -> error "Not found in heap"
                Just v  -> pure v

        Just (StackA addr) ->
            case M.lookup addr stack of
                Nothing -> error "Not found in stack"
                Just v  -> pure v

        Just (StackBool b) ->
            pure $ VBool b

        Just (StackInt i) ->
            pure $ VInt i

evalExpr :: Stringish s => Env s
                        -> NExp s
                        -> State (Machine s) (Val s)

evalExpr env expr =

    case expr of

        AExp aexp ->
            evalAexp env aexp

        CExp cexp ->
            evalCexp env cexp

        NLet a b c -> do
            let Env e = env
            bAddr <- allocChoice =<< evalExpr env b
            let localEnv = Env $ M.insert a bAddr e
            evalExpr localEnv c

evalAexp :: Stringish s => Env s
                        -> AExp s
                        -> State (Machine s) (Val s)
evalAexp env aexp =

    case aexp of

        ATerm t -> evalTerm env t

        AUnPrimOp Negate a ->
            evalAexp env a <&> \case
                VInt i -> VInt (-i)
                _      -> error "Negate expected an Int!"

        AUnPrimOp EShow a ->
            VString . fromString . show <$> evalAexp env a

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

evalTerm :: Stringish s => Env s
                        -> Term s
                        -> State (Machine s) (Val s)
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

        -- hmm, a dcons is just itself, right?
        DCons d     -> pure $ VDCons d

evalCexp :: Stringish s => Env s
                        -> CExp s
                        -> State (Machine s) (Val s)
evalCexp env cexp =

    case cexp of

        CApp f xs -> do

            params <- mapM (evalAexp env) xs

            evalAexp env f >>= \case

                VClo vs body cloEnv -> do

                    -- Preserve the current stack
                    (currentStack, currentStackAddr) <- getStackAndAddr

                    -- Put the parameters onto the stack for the function call
                    allocatedParams <- map StackA <$> mapM allocStack params
                    let vsMap = Env . M.fromList $ zip vs allocatedParams
                    let new = vsMap <> cloEnv <> env -- earlier has precedence
                    val <- evalExpr new body

                    -- Restore the current level of stack
                    putStackAndAddr currentStack currentStackAddr

                    pure val

                VDCons x -> do

                    allocatedParams <- mapM allocChoice params

                    pure $ VDConsApp x allocatedParams

        CIfThenElse pr tr fl ->
            evalAexp env pr >>= \case
                VBool b ->
                    if b
                        then evalExpr env tr
                        else evalExpr env fl
                _ -> error "Mistyped predicate"

putStackAndAddr :: Stack s
                -> StackAddr
                -> State (Machine s) ()
putStackAndAddr st addr = modify' $ \machine -> machine { getStack     = st
                                                        , getStackFree = addr }

getStackAndAddr :: State (Machine s) (Stack s, StackAddr)
getStackAndAddr = do
    machine <- get
    pure (getStack machine, getStackFree machine)

bindTopLevels :: (Ord s, Show s) => [(s, NExp s)]
                                 -> State (Machine s) (Env s)
bindTopLevels = foldM step (Env mempty)
    where
    step (Env env) (n, e) = do
        addr <- StaticA <$> allocStatic (topLevelToVal e)
        pure . Env $ M.insert n addr env

    topLevelToVal :: Ord s => NExp s -> Val s
    topLevelToVal expr =
        case expr of
            AExp (ALam vs body) -> VClo vs body (Env mempty)
            _ -> VClo [] expr (Env mempty)

allocChoice :: Show s => Val s
                      -> State (Machine s) Ptr
allocChoice val = do

    ptr <- go val
    machine <- get
    trace (renderHeapStack machine) $ pure ptr

    where
    go val =
        case val of

            VBool b ->
                pure $ StackBool b

            VInt i ->
                pure $ StackInt i

            VString{} ->
                HeapA <$> allocHeap val

            VClo _ _ (Env env)
                | M.null env -> StaticA <$> allocStatic val -- TODO should have already been called at init?
                | otherwise  -> HeapA <$> allocHeap val

            VDCons{} ->
                StaticA <$> allocStatic val -- TODO check it's not already allocated?

            VDConsApp{} ->
                HeapA <$> allocHeap val

allocHeap :: Show s => Val s
                    -> State (Machine s) HeapAddr
allocHeap v = do
    machine <- get
    let freeHeap  = getFree machine
        freeHeap' = next freeHeap
        Heap heap = getHeap machine
        heap'     = Heap $ M.insert freeHeap v heap
    put $ machine { getHeap = heap'
                  , getFree = freeHeap' }
    pure freeHeap

allocStack :: Show s => Val s
                     -> State (Machine s) StackAddr
allocStack v = do
    machine <- get
    let stackFree   = getStackFree machine
        stackFree'  = next' stackFree
        Stack stack = getStack machine
        stack'      = Stack $ M.insert stackFree v stack
    put $ machine { getStack = stack'
                  , getStackFree = stackFree' }
    pure stackFree

allocStatic :: Show s => Val s
                      -> State (Machine s) StaticAddr
allocStatic v = do
    machine <- get
    let staticFree    = getStaticFree machine
        staticFree'   = next'' staticFree
        Static static = getStatic machine
        static'       = Static $ M.insert staticFree v static
    put $ machine { getStatic = static'
                  , getStaticFree = staticFree' }
    pure staticFree

renderHeapStack :: Show s => Machine s -> String
renderHeapStack machine =
    let Stack stack = getStack machine
        Heap heap   = getHeap machine
    in unlines ["Stack:", renderMap stack, "Heap:", renderMap heap]

renderMap :: (Show k, Show v) => Map k v -> String
renderMap m = unlines
            . map (\(k, v) -> show k ++ " -> " ++ show v)
            $ M.toList m

next :: HeapAddr -> HeapAddr
next (HeapAddr a) = HeapAddr (a+1)

next' :: StackAddr -> StackAddr
next' (StackAddr a) = StackAddr (a+1)

next'' :: StaticAddr -> StaticAddr
next'' (StaticAddr a) = StaticAddr (a+1)

bothM :: Applicative m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = (,) <$> f x <*> f y

