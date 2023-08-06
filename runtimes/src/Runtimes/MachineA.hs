{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Runtimes.MachineA ( bytesToInt
                         , intToBytes
                         , runMachineA ) where

import Common.EitherT
import Common.State
import Common.Trans
import Core.Operator
import Phase.CodeGen.TypesA

import           Data.Bits (shiftL, shiftR)
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as C8
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Vector.Unboxed ((!), Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word8)

data AState s =
    AState { _program   :: !(IntMap (AInstr s))
           , _stack     :: ![SVal]
           , _store     :: !(Map SVal (Vector Word8))
           , _nextStore :: !Int
           , _ip        :: !Int
           , _ipStack   :: ![Int]
           , _registers :: !(Map SVal SVal)
           , _cmp       :: !Bool
           } deriving Show

type Ma a =
    EitherT ByteString (State (AState ByteString)) a

runMachineA :: [AInstr ByteString] -> ByteString
runMachineA instrs = do

    let state = AState { _program   = I.fromList (zip [0..] instrs)
                       , _stack     = []
                       , _store     = mempty
                       , _nextStore = 0
                       , _ip        = 0
                       , _ipStack   = []
                       , _registers = mempty
                       , _cmp       = True
                       }

    let (eResult, state') = runState (runEitherT run) state

    case eResult of
        Left l  -> l
        Right r -> C8.unlines [r, "", pack $ show state']

run :: Ma ByteString
run = do
    setIp =<< resolveLabel "main"
    step

    where
    step = getInstr >>= \case

        ALabel _ -> do
            modifyIp (+1)
            step

        AMov dst src -> do
            move dst src
            modifyIp (+1)
            step

        AMovToPtrOff p@VirtRegPtr{} off sval -> do

            p'     <- readReg p
            sval'  <- evalOnce sval
            sval'' <- intFromSval sval'

            -- warn cast
            let sval''' = intToBytes $ fromIntegral sval''
            let offs   = V.iterateN (V.length sval''') (+1) off
            let updates = V.zip offs sval'''

            lift . modify' $ \state -> do
                let store    = _store state
                let Just mem = M.lookup p' store
                let mem'     = V.update mem updates
                let store'   = M.insert p' mem' store
                state { _store = store'
                      , _ip    = _ip state + 1 }
            step

        -- destreg / offbytes / arg1
        -- Read one word, I guess?
        AMovFromPtrOff dst@VirtRegPrim{} off src@VirtRegPtr{} ->

            readReg src >>= \case

                RMemAddress readAddr -> do
                    state <- lift get
                    case M.lookup (RMemAddress readAddr) (_store state) of
                        Nothing -> left "Read unwritten memory"
                        Just mem -> do
                            let oneWord = bytesToInt . V.take 8 . V.drop off $ mem
                            writeReg dst (RLitInt $ fromIntegral oneWord)
                            modifyIp (+1)
                            step

                _ -> left "AMovFromPtrOff: not an address"

        ABinOp dst op a b -> do
            binOp dst op a b
            modifyIp (+1)
            step

        ACmpB a -> do
            cmp a
            modifyIp (+1)
            step

        Push _ _ val -> do
            push val
            modifyIp (+1)
            step

        Pop _ _ dst -> do
            move dst =<< pop
            modifyIp (+1)
            step

        Call lbl -> do
            ip <- getIp
            pushIp (ip + 1)
            setIp =<< resolveLabel lbl
            step

        Ret val -> do
            push val
            popIp >>= \case
                Nothing ->
                    pop >>= \case
                        i@VirtRegPrim{} -> do
                            x <- readReg i
                            pure . pack . show $ x -- is this a good exit
                        RLitInt i     -> pure . pack $ show i -- is this a good exit?
                Just ip -> do
                    setIp ip
                    step

        J lbl -> do
            j lbl
            step

        Je lbl -> do
            je lbl
            step

        Jne lbl -> do
            jne lbl
            step

        AComment{} -> do
            modifyIp (+1)
            step

        Allocate dst@VirtRegPtr{} sz -> do

            state <- lift get

            let nextStore = _nextStore state

            -- allocate space
            let mem = V.replicate sz (0 :: Word8)
            let store = _store state
            let store' = M.insert (RMemAddress nextStore) mem store

            -- Point to the heap
            lift $ put state { _store     = store'
                             , _nextStore = nextStore + sz
                             , _ip        = _ip state + 1 }

            move dst (RMemAddress nextStore)

            step

        AErr err ->
            left err

        inst -> left $ "Unkn instr: " <> pack (show inst)

--warn cast
intFromSval :: SVal -> Ma Int
intFromSval (RLitInt i) = pure $ fromIntegral i
intFromSval _ = left "intFromSval"

-- TODO test negatives, or only use Word64s
intToBytes :: Int -> Vector Word8
intToBytes i = go [] 0
    where
    go acc 64 = V.fromListN 8 (reverse acc)
    go acc n  = go (fromIntegral (shiftR i n) : acc) (n+8)

-- TODO test negatives, or only use Word64s
bytesToInt :: Vector Word8 -> Int
bytesToInt bs = go 0 0
    where
    go acc 8 = acc
    go acc n = go (acc + ((fromIntegral (bs ! n)) `shiftL` (n*8))) (n+1)

resolveLabel :: ByteString -> Ma Int
resolveLabel lbl =
    (map fst . filter (\(_, x) -> x == ALabel lbl) . I.toList . _program <$> lift get) >>= \case
        []  -> left $ "No such label: " <> lbl
        [x] -> pure x
        _   -> left "Too many labels"

modifyIp :: (Int -> Int) -> Ma ()
modifyIp f = lift . modify' $ \ps -> ps { _ip = f $ _ip ps }

setIp :: Int -> Ma ()
setIp ip = lift . modify' $ \ps -> ps { _ip = ip }

getIp :: Ma Int
getIp = _ip <$> lift get

move :: SVal -> SVal -> Ma ()
move dst@VirtRegPrim{} src               = writeReg dst =<< evalOnce src
move dst@VirtRegPtr{}  src@VirtRegPtr{}  = writeReg dst src -- guess.  both ptrs, shouldn't need eval?
move dst@VirtRegPtr{}  src@RMemAddress{} = writeReg dst src
move a b = left . pack . show $ ("move", a, b)

writeReg :: SVal -> SVal -> Ma ()
writeReg dst src = lift . modify' $ \ps -> ps { _registers = M.insert dst src (_registers ps) }

readReg :: SVal -> Ma SVal
readReg reg = do
    ps <- lift get
    case M.lookup reg (_registers ps) of
        Nothing -> do
            let ip = _ip ps
                p  = _program ps
                i  = I.lookup ip p
            left $ "No such reg: " <> pack (show reg) <> " at ip " <> pack (show ip) <> ": " <> pack (show i)
        Just v  -> pure v

pushIp :: Int -> Ma ()
pushIp ip = lift . modify' $ \ps -> ps { _ipStack = ip : _ipStack ps }

popIp :: Ma (Maybe Int)
popIp = do
    ipStack <- _ipStack <$> lift get
    case ipStack of
        [] -> pure Nothing
        (i:pStack) -> do
            lift . modify' $ \ps -> ps { _ipStack = pStack }
            pure $ Just i

evalOnce :: SVal -> Ma SVal
evalOnce v@VirtRegPrim{} = readReg v
evalOnce v@RLitBool{}    = pure v
evalOnce v@RLitInt{}     = pure v

binOp :: SVal -> BinOp -> SVal -> SVal -> Ma ()
binOp dst op a b = do

    va <- evalOnce a >>= \case
            RLitInt va -> pure va
            _          -> left "Not an RLitInt"

    vb <- evalOnce b >>= \case
            RLitInt vb -> pure vb
            _          -> left "Not an RLitInt"

    vc <- case op of
              MulI -> pure . RLitInt  $! va  * vb
              AddI -> pure . RLitInt  $! va  + vb
              SubI -> pure . RLitInt  $! va  - vb
              EqA  -> pure . RLitBool $! va == vb   -- probably means the above can be not just an int
              _    -> left $ "unknown op: " <> pack (show op)

    move dst vc

cmp :: SVal -> Ma ()
cmp val =
    evalOnce val >>= \case
        RLitBool b -> lift . modify' $ \ps -> ps { _cmp = b }
        x          -> left $ "Not a comparable: " <> pack (show x)

push :: SVal -> Ma ()
push val@RLitInt{}     = lift $ modify' $ \ps -> ps { _stack = val : _stack ps }
push val@RMemAddress{} = lift $ modify' $ \ps -> ps { _stack = val : _stack ps }
push val@VirtRegPrim{} = readReg val >>= push
push val@VirtRegPtr{}  = readReg val >>= push
push x                 = left . pack $ "push: " ++ show x

pop :: Ma SVal
pop = do
    stack <- _stack <$> lift get
    case stack of
        [] -> left "empty stack"
        (s:tack) -> lift $ do
            modify' $ \ps -> ps { _stack = tack }
            pure s

j :: ByteString -> Ma ()
j lbl = setIp =<< resolveLabel lbl

je :: ByteString -> Ma ()
je lbl = do
    cmp' <- _cmp <$> lift get
    if cmp'
        then setIp =<< resolveLabel lbl
        else modifyIp (+1)

jne :: ByteString -> Ma ()
jne lbl = do
    cmp' <- _cmp <$> lift get
    if cmp'
        then modifyIp (+1)
        else setIp =<< resolveLabel lbl

getInstr :: Ma (AInstr ByteString)
getInstr = do
    ps <- lift get
    case I.lookup (_ip ps) (_program ps) of
        Nothing -> left "no such instruction"
        Just i  -> pure i
