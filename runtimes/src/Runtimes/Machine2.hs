{-# LANGUAGE OverloadedStrings #-}

module Runtimes.Machine2 (runMachine2) where

import Core.Operator
import Common.State
import Phase.CodeGen.CodeGen0
import Phase.CodeGen.SizeInfo -- TODO should this be compile-time only?
import Phase.CodeGen.Val

import           Data.ByteString             (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Map.Strict             (Map)
import qualified Data.Map as M

import           Data.Vector ((!), Vector)
import qualified Data.Vector          as V

data MachineState s =
    MachineState { stack   :: [Val s]
                 , regs    :: Map s (Val s)
                 , ipStack :: [Int]
                 , ip      :: Int
                 , mcmp    :: Maybe Comparison
                 }

data Comparison = Ceq | CNeq deriving Show

runMachine2 :: [Instr ByteString] -> IO ByteString
runMachine2 is =
    let vis = V.fromList is
    in case findLbl "main" vis of
           Nothing -> pure "No main found"
           Just ip -> go (MachineState [] mempty [-1] ip Nothing) vis

-- TODO convert to state monad
go :: MachineState ByteString -> Vector (Instr ByteString) -> IO ByteString
go ms vis = do

    let MachineState stack regs ipstack ip mCmp = ms
    
    let i = vis ! ip

    print i

    case i of

        ILabel{} ->
            go (MachineState stack regs ipstack (ip + 1) mCmp) vis

        Push val -> do
            let stack' = eval regs val:stack
            print ("Stack", stack')
            go (MachineState stack' regs ipstack (ip + 1) mCmp) vis
        
        PopTyped _ r -> do
            let (s:tack) = stack
            go (MachineState tack (M.insert r s regs) ipstack (ip + 1) mCmp) vis

        Ret -> do
            let (ip':ipstack') = ipstack
            if ip' == -1
                then pure . C8.pack . show . eval regs $ head stack
                else go (MachineState stack regs ipstack' ip' mCmp) vis

        Assign dst a -> do
            let a' = eval regs a
            go (MachineState stack (M.insert dst a' regs) ipstack (ip + 1) mCmp) vis

        BinOpInstr AddI dst a b -> do
            let VInt a' = eval regs a
                VInt b' = eval regs b
                c       = VInt $! a' + b'
            go (MachineState stack (M.insert dst c regs) ipstack (ip + 1) mCmp) vis

        BinOpInstr SubI dst a b -> do
            let VInt a' = eval regs a
                VInt b' = eval regs b
                c       = VInt $! a' - b'
            go (MachineState stack (M.insert dst c regs) ipstack (ip + 1) mCmp) vis

        CallFun (Label f) -> do 
            let ipstack' = ip + 1: ipstack
                Just ip' = findLbl f vis
            print ("ipstack", ipstack')
            go (MachineState stack regs ipstack' ip' mCmp) vis

        CallFun f -> do
            let LabelPos ip' = eval regs f
                ipstack' = ip + 1: ipstack
            print ("ipstack", ipstack')
            go (MachineState stack regs ipstack' ip' mCmp) vis

        BinOpInstr EqA dst a b -> do
            let a'    = eval regs a
                b'    = eval regs b
                e     = vEq a' b'
                regs' = M.insert dst e regs
            go (MachineState stack regs' ipstack (ip + 1) mCmp) vis

        Cmp r -> do
            let VBool r' = eval regs r
                cmp = Just $ if r' then Ceq else CNeq
            go (MachineState stack regs ipstack (ip + 1) cmp) vis

        JmpNeqLbl lbl ->
            case mcmp ms of
                Nothing    -> error "foo"
                Just Ceq  -> go (MachineState stack regs ipstack (ip+1) mCmp) vis
                Just CNeq -> do
                    let LabelPos ip' = eval regs (Label lbl)
                    go (MachineState stack regs ipstack ip' mCmp) vis

        JmpLbl lbl -> do
            let LabelPos ip' = eval regs (Label lbl)
            go (MachineState stack regs ipstack ip' mCmp) vis

        _ -> error $ "^ Unknown instruction ^"

    where
    vEq (VInt a)  (VInt b)  = VBool $! a == b
    vEq (VBool a) (VBool b) = VBool $! a == b

    eval    _    (Label lbl) = let Just i = findLbl lbl vis in LabelPos i
    eval    _   lp@LabelPos{} = lp
    eval    _       v@VInt{} = v
    eval    _      v@VBool{} = v
    eval regs (TypedReg _ r) =
        case M.lookup r regs of
            Nothing -> error $ "Not found: " ++ show r
            Just v  -> v -- eval regs v
    eval regs x = error $ show ("unknown", x)

findLbl :: ByteString -> Vector (Instr ByteString) -> Maybe Int
findLbl lbl = V.headM
            . V.map fst
            . V.filter (isLabel . snd)
            . V.imap ((,))
    where
    isLabel (ILabel lbl') = lbl == lbl'
    isLabel             _ = False
