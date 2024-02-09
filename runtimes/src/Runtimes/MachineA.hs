{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Runtimes.MachineA ( bytesToInt
                         , intToBytes
                         , runMachineA ) where

import Common.EitherT
import Common.State
import Common.Trans
import Core.Operator
import Core.Types
import Phase.CodeGen.TypesA

import           Data.Bits (shiftL, shiftR)
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as C8
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           Data.Vector.Unboxed ((!), Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word8)

data AState s =
    AState { _program   :: !(IntMap (AInstr s))
           , _stack     :: ![AVal]
           , _store     :: !(IntMap (Vector Word8))
           , _nextStore :: !Int
           , _ip        :: !Int
           , _ipStack   :: ![Int]
           , _registers :: !(IntMap AVal)
           , _cmp       :: !Bool
           , _debugLog  :: ![ByteString]
           } deriving Show

type Ma a =
    EitherT ByteString (
        State (AState ByteString)) a

runMachineA :: [AInstr ByteString] -> (ByteString, ByteString)
runMachineA instrs = do

    let state = AState { _program   = I.fromList (zip [0..] instrs)
                       , _stack     = []
                       , _store     = mempty
                       , _nextStore = 0
                       , _ip        = 0
                       , _ipStack   = []
                       , _registers = mempty
                       , _cmp       = True
                       , _debugLog  = mempty
                       }

    let (eResult, state') = runState (runEitherT run) state

    case eResult of
        Left l  -> (C8.unlines . reverse $ _debugLog state', l)
        Right r -> ("No error", C8.unlines [r, "", pack $ show state'])

run :: Ma ByteString
run = do
    setIp =<< resolveLabel "main"
    step

    where
    step = getInstr >>= \case

        AComment _ -> do
            modifyIp (+1)
            step

        ALabel _ -> do
            modifyIp (+1)
            step

        Ret val ->
            ret val step

        AMov _ (RegFromLitInt dst li) -> do
            writeReg dst (ALitInt li)
            modifyIp (+1)
            step

        AMov _ (RegFromReg dst src) -> do
            writeReg dst =<< readReg src
            modifyIp (+1)
            step

        ABinOp dest op ta a tb b -> do
            binOp dest op ta a tb b
            modifyIp (+1)
            step

        -- Unused dbg comment & type
        Push _ _ a -> do
            push a
            modifyIp (+1)
            step

        Pop _ _ dst -> do
            writeReg dst =<< pop
            modifyIp (+1)
            step

        Call lbl _ _ -> do
            ip <- getIp
            pushIp (ip + 1)
            setIp =<< resolveLabel lbl
            step

        ACmpB a -> do
            cmp a
            modifyIp (+1)
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

        Allocate dst sz -> do
            alloc dst sz
            modifyIp (+1)
            step

        AMov _ mm -> do
            move mm
            modifyIp (+1)
            step

        x ->
            left $ "step: " <> pack (show x)

cmp :: AVal -> Ma ()
cmp val = do

    val' <- case val of
                ALitBool b -> pure b
                AReg r     -> readReg r >>= \case
                    ALitBool b -> pure b
                    _          -> left "cmp a: not a bool"
                _          -> left "cmp b: not a bool"

    lift . modify' $ \ps -> ps { _cmp = val' }

-- This is probably too much computation for a simple interpreter
-- Split into typed variants
binOp :: Int -> BinOp -> Type ByteString -> AVal -> Type ByteString -> AVal -> Ma () 
binOp dest op ta a tb b = 

    case (op, ta, tb) of 

        (AddI, TyCon "Int" [], TyCon "Int" []) -> do
            a' <- asInteger a
            b' <- asInteger b
            writeReg dest (ALitInt $! a' + b')

        (SubI, TyCon "Int" [], TyCon "Int" []) -> do
            a' <- asInteger a
            b' <- asInteger b
            writeReg dest (ALitInt $! a' - b')

        (MulI, TyCon "Int" [], TyCon "Int" []) -> do
            a' <- asInteger a
            b' <- asInteger b
            writeReg dest (ALitInt $! a' * b')

        (EqA, TyCon "Bool" [], TyCon "Bool" []) -> do
            a' <- asBool a
            b' <- asBool b
            writeReg dest (ALitBool $! a' == b')

        (EqA, TyCon "Int" [], TyCon "Int" []) -> do
            a' <- asInteger a
            b' <- asInteger b
            writeReg dest (ALitBool $! a' == b')

        (AndB, TyCon "Bool" [], TyCon "Bool" []) -> do
            a' <- asBool a
            b' <- asBool b
            writeReg dest (ALitBool $! a' && b')

        (OrB, TyCon "Bool" [], TyCon "Bool" []) -> do
            a' <- asBool a
            b' <- asBool b
            writeReg dest (ALitBool $! a' || b')

        (GtI, TyCon "Int" [], TyCon "Int" []) -> do
            a' <- asInteger a
            b' <- asInteger b
            writeReg dest (ALitBool $! a' > b')

        (LtI, TyCon "Int" [], TyCon "Int" []) -> do
            a' <- asInteger a
            b' <- asInteger b
            writeReg dest (ALitBool $! a' < b')

        _ ->
            left . pack $ "binOp: " ++ show (op, ta, tb)

    where
    asBool (ALitBool lb) = pure lb
    asBool (AReg r) = readReg r >>= \case
        ALitBool lb -> pure lb
        _           -> left "Not a bool"

    asInteger (ALitInt li) = pure li
    asInteger (AReg r) = readReg r >>= \case
        ALitInt li -> pure li
        _          -> left "Not an int"

ret :: AVal
    -> Ma ByteString
    -> Ma ByteString
ret val step = do
    push val
    popIp >>= \case

        -- Resume computation from returned-to location
        Just ip -> do
            setIp ip
            step

        -- Reached end of the program
        Nothing ->
            pop >>= \case
                i@ALitInt{}  -> pure . pack . show $ i
                b@ALitBool{} -> pure . pack . show $ b
                AReg r       -> pack . show <$> readReg r
                _            -> left "idk"

alloc :: Int -> Int -> Ma ()
alloc dst sz = lift $ do

    state <- get

    let nextStore = _nextStore state

    -- allocate space
    let mem       = V.replicate sz (0 :: Word8)
        store     = I.insert nextStore mem (_store state)
        registers = I.insert dst (MemAddress nextStore) (_registers state)

    -- Point to the heap
    put state { _store     = store
              , _nextStore = nextStore + sz
              , _registers = registers }

move :: MovMode -> Ma ()
move movMode = do

    state <- lift get
    let registers = _registers state
    let store     = _store state

    -- TODO dedupe a little
    case movMode of

        MemFromReg dst off src ->
            let Just (MemAddress addr) = I.lookup dst registers
                Just mem               = I.lookup addr store
                (start, mid)           = V.splitAt off mem
                Just val               = I.lookup src registers
                bytes                  = valToBytes val
                end                    = V.drop (V.length bytes) mid
                mem'                   = mconcat [start, bytes, end]
                store'                 = I.insert addr mem' store
            in lift $ put state { _store = store' }

        MemFromLitInt dst off li ->
            let Just (MemAddress addr) = I.lookup dst registers
                Just mem               = I.lookup addr store
                (start, mid)           = V.splitAt off mem
                liBytes                = intToBytes (fromIntegral li)
                end                    = V.drop (V.length liBytes) mid
                mem'                   = mconcat [start, liBytes, end]
                store'                 = I.insert addr mem' store
            in lift $ put state { _store = store' }

        MemFromLitBool dst off lb -> -- TODO untested
            let Just (MemAddress addr) = I.lookup dst registers
                Just mem               = I.lookup addr store
                (start, mid)           = V.splitAt off mem
                liBytes                = intToBytes $ if lb then 1 else 0
                end                    = V.drop (V.length liBytes) mid
                mem'                   = mconcat [start, liBytes, end]
                store'                 = I.insert addr mem' store
            in lift $ put state { _store = store' }

        RegFromMem dst src off ->
            let Just regVal            = I.lookup src registers
                addr                   = bytesToAddr regVal
                Just mem               = I.lookup addr store
                bytes                  = V.take 8 (V.drop off mem)
                val                    = fromIntegral $ bytesToInt bytes
                registers'             = I.insert dst (ALitInt val) registers
            in lift $ put state { _registers = registers' }

        _ -> left $ "unknown movmode: " <> (pack $ show movMode)

-- fast and loose
bytesToAddr :: AVal -> Int
bytesToAddr (ALitInt i) = fromIntegral i
bytesToAddr (MemAddress a) = a

-- A bit fast-and-loose here (nested data structures)
valToBytes (ALitInt li) = intToBytes (fromIntegral li)
valToBytes (MemAddress a) = intToBytes a
valToBytes v = error $ show v

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

writeReg :: Int -> AVal -> Ma ()
writeReg dst src = lift . modify' $ \ps -> ps { _registers = I.insert dst src (_registers ps) }

readReg :: Int -> Ma AVal
readReg reg = do
    ps <- lift get
    case I.lookup reg (_registers ps) of
        Nothing -> do
            let ip = _ip ps
                p  = _program ps
                i  = I.lookup ip p
            left $ "No such reg: " <> pack (show reg) <> " at ip " <> pack (show ip) <> ": " <> pack (show i)
        Just v  -> pure v

resolveLabel :: ByteString -> Ma Int
resolveLabel lbl =
    (map fst . concatMap getLabel . I.toList . _program <$> lift get) >>= \case
        []  -> left $ "No such label: " <> lbl
        [x] -> pure x
        _   -> left "Too many labels"
    where
    getLabel x@(_, ALabel lbl') | lbl == lbl' = [x]
    getLabel _                                = []

setIp :: Int -> Ma ()
setIp ip = lift . modify' $ \ps -> ps { _ip = ip }

getIp :: Ma Int
getIp = _ip <$> lift get

pushIp :: Int -> Ma ()
pushIp ip = lift . modify' $ \ps -> ps { _ipStack = ip : _ipStack ps }

modifyIp :: (Int -> Int) -> Ma ()
modifyIp f = lift . modify' $ \ps -> ps { _ip = f $ _ip ps }

popIp :: Ma (Maybe Int)
popIp = do
    ipStack <- _ipStack <$> lift get
    case ipStack of
        [] -> pure Nothing
        (i:pStack) -> do
            lift . modify' $ \ps -> ps { _ipStack = pStack }
            pure $ Just i

-- Eval anything before it goes onto the stack,
-- that way we always pop off a proper value (not a register)
push :: AVal -> Ma ()
push val = do
    val' <- case val of
                b@ALitBool{} -> pure b
                i@ALitInt{}  -> pure i
                AReg r       -> readReg r
    lift $ modify' $ \ps -> ps { _stack = val' : _stack ps }

pop :: Ma AVal
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
        Just i  -> do
            let _debugLog' =
                    case i of
                        ALabel{}   -> pack (show i) : []
                        AComment{} ->                 _debugLog ps
                        _          -> pack (show i) : _debugLog ps
            lift $ put ps { _debugLog = _debugLog' }
            pure i
