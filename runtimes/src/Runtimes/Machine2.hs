{-# LANGUAGE LambdaCase #-}

module Runtimes.Machine2 (runMachine2) where

import Core.Operator
import Common.StateT
import Phase.CodeGen.CodeGen0

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor                ((<&>))
import           Data.Map.Strict             (Map)
import qualified Data.Map as M
import           Data.Vector ((!), Vector)
import qualified Data.Vector          as V
import           UnliftIO.Exception

data MachineState s =
    MachineState { getStack   :: [Val s]
                 , getRegs    :: Map s (Val s)
                 , getIpStack :: [Int]
                 , getIp      :: Int
                 , getMCmp    :: Maybe Comparison
                 }

data Comparison = Ceq | CNeq

runMachine2 :: [Instr ByteString] -> IO (Either ByteString ByteString)
runMachine2 is =
    let vis = V.fromList is
        ip  = findLbl vis (C8.pack "main")
    in
    tryAnyDeep (fst <$> runStateT (run vis) (initialMachine ip)) <&> \case
        Left er -> Left . C8.pack $ show er
        Right r -> Right r

initialMachine :: Ord s => Int -> MachineState s
initialMachine ip =
    MachineState { getStack   = []
                 , getRegs    = mempty
                 , getIpStack = [-1]
                 , getIp      = ip
                 , getMCmp    = Nothing
                 }

incIp :: Monad m => StateT (MachineState s) m ()
incIp = modifyt $ \ms -> ms { getIp = getIp ms + 1 }

push :: Monad m => Val s -> StateT (MachineState s) m ()
push v = modifyt $ \ms -> ms { getStack = v : getStack ms }

pushIp :: Monad m => Int -> StateT (MachineState s) m ()
pushIp ip = modifyt $ \ms -> ms { getIpStack = ip : getIpStack ms }

pop :: Monad m => StateT (MachineState s) m (Val s)
pop = do
    ms <- gett
    case getStack ms of
        [] -> error "Stack underflow"
        (s:tack) -> do
            putt ms { getStack = tack }
            pure s

popIp :: Monad m => StateT (MachineState s) m Int
popIp = do
    ms <- gett
    case getIpStack ms of
        [] -> error "Ip underflow"
        (i:pStack) -> do
            putt ms { getIpStack = pStack }
            pure i

setCmp :: Monad m => Comparison -> StateT (MachineState s) m ()
setCmp cmp = modifyt $ \ms -> ms { getMCmp = Just cmp }

setReg :: (Ord s, Monad m) => s -> Val s -> StateT (MachineState s) m ()
setReg r v = modifyt $ \ms -> ms { getRegs = M.insert r v (getRegs ms) }

jmpTo :: Monad m => Int -> StateT (MachineState s) m ()
jmpTo ip = modifyt $ \ms -> ms { getIp = ip }

run :: Vector (Instr ByteString) -> StateT (MachineState ByteString) IO ByteString
run vis = go

  where
  go = do

    (ip, regs) <- gett <&> \ms -> (getIp ms, getRegs ms)

    case (vis ! ip) of

        ILabel{} -> do
            incIp
            go

        Push val -> do
            push =<< eval regs val
            incIp
            go

        PopTyped _ r -> do
            setReg r =<< pop
            incIp
            go

        Ret -> do
            ip' <- popIp
            if ip' == -1
                then do
                    s <- pop
                    regs <- getRegs <$> gett
                    C8.pack . show <$> eval regs s
                else do
                    jmpTo ip'
                    go

        Assign dst a -> do
            setReg dst =<< eval regs a
            incIp
            go

        BinOpInstr op dst a b -> do
            a' <- eval regs a
            b' <- eval regs b
            setReg dst $! binOp op a' b'
            incIp
            go

        CallFun f -> do
            LabelPos ip' <- eval regs f
            pushIp $! ip + 1
            jmpTo ip'
            go

        Cmp r -> do
            VBool r' <- eval regs r
            setCmp $! if r' then Ceq else CNeq
            incIp
            go

        JmpNeqLbl lbl -> do

            cmp <- getMCmp <$> gett

            case cmp of

                Nothing ->
                    error "foo"

                Just Ceq -> do
                    incIp
                    go

                Just CNeq -> do
                    jmpTo $! findLbl vis lbl
                    go

        JmpLbl lbl -> do
            jmpTo $! findLbl vis lbl
            go

        IComment{} -> do
            incIp
            go

        x -> error $ "^ Unknown instruction ^: " ++ show x

    where
    eval    _   (Label lbl)  = pure . LabelPos $! findLbl vis lbl
    eval    _  lp@LabelPos{} = pure lp
    eval    _      v@VInt{}  = pure v
    eval    _     v@VBool{}  = pure v
    eval regs (TypedReg _ r) =
        case M.lookup r regs of
            Nothing -> do
                ip <- getIp <$> gett
                error $ concat ["Not found: ", show r, " at ip ", show ip]
            Just v -> pure v -- eval regs v

    eval regs (UntypedReg r) =
        case M.lookup r regs of
            Nothing -> do
                ip <- getIp <$> gett
                error $ concat ["Not found: ", show r, " at ip ", show ip]
            Just v  -> pure v -- eval regs v

    eval _ x = error $ show ("unknown", x)

binOp :: Show s => BinOp -> Val s -> Val s -> Val s
binOp AddI  (VInt a)  (VInt b) = VInt $! a + b
binOp SubI  (VInt a)  (VInt b) = VInt $! a - b
binOp MulI  (VInt a)  (VInt b) = VInt $! a * b
binOp EqA   (VInt a)  (VInt b) = VBool $! a == b
binOp EqA  (VBool a) (VBool b) = VBool $! a == b
binOp a b c                    = error $ show ("binop", a, b, c)

findLbl :: (Eq s, Show s) => Vector (Instr s) -> s -> Int
findLbl vis lbl =
    case findLbl' vis of
        Nothing -> error $ "No such label: " ++ show lbl
        Just ip -> ip

    where
    findLbl' = V.headM
             . V.map fst
             . V.filter (isLabel . snd)
             . V.imap ((,))
        where
        isLabel (ILabel lbl') = lbl == lbl'
        isLabel             _ = False
