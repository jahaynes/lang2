{-# LANGUAGE OverloadedStrings #-}

module Runtimes.Machine1 where

import Core.Operator
import Common.State
import Phase.CodeGen.CodeGen0
import Phase.CodeGen.CodeGen1

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Map.Strict             (Map)
import qualified Data.Map as M
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector as V
import           Debug.Trace                 (trace)

data Machine1 s =
    Machine1 { getCode      :: !(Vector (Instr s))
             , getStack     :: ![Val s]
             , getCallStack :: ![Int]
             , getIp        :: !Int
             , getRegisters :: !(Map s (Val s))
             } deriving Show

runMachine1 :: [Instr ByteString] -> ByteString
runMachine1 is = do

    let machine = Machine1 { getCode      = V.fromList is
                           , getStack     = []
                           , getCallStack = [-1]
                           , getIp        = 0
                           , getRegisters = mempty
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

            CallFun (VLoc v) -> do
                pushIp (ip + 1)
                setIp v
                go

            Ret -> do
                ip' <- popIp
                if ip' == -1
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
                    (AddI, VInt a'', VInt b'') -> setReg dst (VInt $! a'' + b'')
                    (MulI, VInt a'', VInt b'') -> setReg dst (VInt $! a'' * b'')
                setIp (ip + 1)
                go

            Assign dst a -> do
                a' <- eval a
                setReg dst a'
                setIp (ip + 1)
                go

            _ -> error . show $ code ! ip

eval :: (Ord s, Show s) => Val s -> State (Machine1 s) (Val s)
eval v =

    case v of

        Reg r -> do
            registers <- getRegisters <$> get
            case M.lookup r registers of
                Just v  -> pure v
                Nothing -> error "missing register"

        VInt{} ->
            pure v

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