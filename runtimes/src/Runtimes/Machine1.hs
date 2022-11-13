{-# LANGUAGE OverloadedStrings #-}

module Runtimes.Machine1 where

import Core.Operator
import Common.State
import Phase.CodeGen.CodeGen0
import Phase.CodeGen.SizeInfo -- TODO should this be compile-time only?

import           Data.ByteString             (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Map.Strict             (Map)
import qualified Data.Map as M
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector as V
import           Debug.Trace                 (trace)
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VM
import           Control.Monad.ST
import           Data.Serialize     (encode)
import           Data.Word (Word8)

newtype HeapAddr =
    HeapAddr Int
        deriving (Show, Eq, Ord)

data Machine1 s =
    Machine1 { getCode       :: !(Vector (Instr s))
             , getStack      :: ![Val s]
             , getCallStack  :: ![Int]
             , getIp         :: !Int
             , getRegisters  :: !(Map s (Val s))
             , getComparison :: !Bool
             , getLinkerInfo :: !(Map s Int)                       -- Labels to positions
             , getHeap       :: !(Map HeapAddr (VS.Vector Word8))  -- address to bytes
             , getNextFree   :: !HeapAddr                          -- where does this live? OS?
             } deriving Show

-- TODO space leak of pos?
extractLocations :: (Int, Int, Map ByteString Int) -> Instr ByteString -> (Int, Int, Map ByteString Int)
extractLocations (mainPos, pos, acc) instr =

    case instr of

        ILabel lbl ->
            let mainPos' = if lbl == "main" then pos else mainPos
                acc' = M.insert lbl pos acc
            in (mainPos', pos+1, acc')
                      
        _ -> (mainPos, pos+1, acc)

runMachine1 :: [Instr ByteString] -> ByteString
runMachine1 is = do

    let code = V.fromList is

        (mainPos, _, linkerInfo) = V.foldl' extractLocations (0, 0, mempty) code

        machine = Machine1 { getCode       = code
                           , getStack      = []
                           , getCallStack  = [-1]
                           , getIp         = mainPos
                           , getRegisters  = mempty
                           , getComparison = False
                           , getLinkerInfo = linkerInfo
                           , getHeap       = mempty
                           , getNextFree   = HeapAddr 0
                           }

    C8.pack . show $ evalState go machine

    where
    go :: (Ord s, Show s) => State (Machine1 s) (Val s)
    go = do

        machine <- get

        let code = getCode machine
            ip   = getIp   machine

        case code ! ip of

            Push v -> do
                push v
                setIp (ip + 1)
                go

            CallFun v -> do
                Label lbl <- eval v
                linkerInfo <- getLinkerInfo <$> get
                let Just loc = M.lookup lbl linkerInfo
                pushIp (ip + 1)
                setIp loc
                go

            Ret -> do
                ip' <- popIp
                if ip' == -1 -- Done signal
                    then pop >>= eval
                    else do
                        setIp ip'
                        go

            Pop r -> do
                v <- pop
                setReg r v
                setIp (ip + 1)
                go

            BinOpInstr op dst a b -> do
                a' <- eval a
                b' <- eval b
                case (op, a', b') of
                    (AddI,  VInt a'',  VInt b'') -> setReg dst (VInt  $! a''     + b'')
                    (SubI,  VInt a'',  VInt b'') -> setReg dst (VInt  $! a''     - b'')
                    (MulI,  VInt a'',  VInt b'') -> setReg dst (VInt  $! a''     * b'')
                    (DivI,  VInt a'',  VInt b'') -> setReg dst (VInt  $! a'' `div` b'')
                    (EqA,   VInt a'',  VInt b'') -> setReg dst (VBool $! a''    == b'')
                    (AndB, VBool a'', VBool b'') -> setReg dst (VBool $! and [a'', b''])
                    _ -> error $ show op
                setIp (ip + 1)
                go

            Assign dst a -> do
                a' <- eval a
                setReg dst a'
                setIp (ip + 1)
                go

            Cmp r -> do
                VBool r' <- eval r
                modify' $ \m -> m { getComparison = r' }
                setIp (ip + 1)
                go

            ILabel{} -> do
                setIp (ip + 1)
                go

            JmpLbl lbl -> do
                Just loc <- M.lookup lbl . getLinkerInfo <$> get
                setIp loc
                go

            JmpNeqLbl lbl -> do
                m <- get
                if getComparison m
                    then do
                        setIp (ip + 1)
                        go
                    else do
                        let Just loc = M.lookup lbl (getLinkerInfo m)
                        setIp loc
                        go

            -- Generalise as foreign c call?
            Malloc r sz -> do
                HeapAddr addr <- allocate sz
                setReg r (VHPtr addr)
                setIp (ip + 1)
                go

            Cpy dest a ->
                case dest of

                    VAddressAt reg -> error "not implemented" {-
                        VHPtr ptr <- eval (Reg reg)
                        writeTo (HeapAddr ptr) a
                        setIp (ip + 1)
                        go
-}

                    RegPtrOff reg off -> do
                        VHPtr ptr <- eval (Reg reg)
                        writeTo (HeapAddr ptr) off a
                        setIp (ip + 1)
                        go

                    _ -> error $ show (code ! ip)

            IComment _ -> do
                setIp (ip + 1)
                go

            _ ->
                error $ show (code ! ip)

writeTo :: Show s => HeapAddr -> Int -> Val s -> State (Machine1 s) ()
writeTo heapAddr offset val =
    modify' $ \m -> do
        let val' = case val of
                       VInt i -> fromIntegral i :: Int -- TODO truncation issue
                       VTag t -> t :: Int
        let heap     = getHeap m
            Just mem = M.lookup heapAddr heap
            mem'     = runST $ do
                            src <- VS.thaw . VS.fromList . BS.unpack $ encode val'
                            dst <- VS.thaw mem
                            let dstSlice = VM.slice offset (getSize (SizedVal val)) dst
                            VM.copy dstSlice src
                            VS.freeze dst
        m { getHeap = M.insert heapAddr mem' heap }

allocate :: Int -> State (Machine1 s) HeapAddr
allocate sz = do
    m <- get
    let nf@(HeapAddr nextFree) = getNextFree m
        heap = getHeap m
        allocated = VS.replicate sz 0
    put m { getNextFree = HeapAddr $ nextFree + 1
          , getHeap     = M.insert nf allocated heap
          }
    pure nf

eval :: (Ord s, Show s) => Val s -> State (Machine1 s) (Val s)
eval v =

    case v of

        Reg r -> do
            registers <- getRegisters <$> get
            case M.lookup r registers of
                Just v' -> pure v'
                Nothing -> error $ "missing register: " ++ show r

        VBool{} ->
            pure v

        VInt{} ->
            pure v

        Label{} ->
            pure v

        -- Probably shouldn't be here
        VDConsName{} ->
            pure v

        -- Probably shouldn't be here
        VDCons{} ->
            pure v

        _ ->
            error $ show v

push :: (Ord s, Show s) => Val s -> State (Machine1 s) ()
push v = do
    v' <- eval v
    modify' $ \m -> m { getStack = v' : getStack m }

-- TODO check no reassign?
setReg :: Ord s => s -> Val s -> State (Machine1 s) ()
setReg k v = modify' $ \m ->
    let registers = getRegisters m
    in m { getRegisters = M.insert k v registers }

pop :: State (Machine1 s) (Val s)
pop = do
    m <- get
    let s:tack = getStack m
    put m { getStack = tack }
    pure s

pushIp :: Int -> State (Machine1 s) ()
pushIp ip = modify' $ \m -> m { getCallStack = ip : getCallStack m }

popIp :: State (Machine1 s) Int
popIp = do
    m <- get
    let s:tack = getCallStack m
    put m { getCallStack = tack }
    pure s

setIp :: Int -> State (Machine1 s) ()
setIp ip = modify' $ \m -> m { getIp = ip }