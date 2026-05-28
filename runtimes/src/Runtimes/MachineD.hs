{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}

module Runtimes.MachineD ( runMachineD ) where

import Phase.CodeGen.TypesD

import Common.EitherT (EitherT, left, runEitherT)
import Common.ReaderT (ReaderT, ask, runReaderT)
import Common.StateT  (StateT, get, modify, put, runStateT)
import Common.Trans   (lift)

import           Data.ByteString         (ByteString)
import           Data.Functor            ((<&>))
import           Data.Map                (Map)
import qualified Data.Map.Strict as M
import           Data.String.Interpolate (i)
import           Data.Vector             ((!?), Vector)
import qualified Data.Vector as V

type D m a =
    StateT MachineState (
        ReaderT (MachineEnv ByteString) (
            EitherT ByteString IO)) a

runMachineD :: [DInstr ByteString] -> IO (Either ByteString ByteString)
runMachineD is =

    let env = MachineEnv { getInstrs = V.fromList is }

    in runEitherT (runReaderT initAndRun env) <&> \case
          Left s       -> Left s
          Right (x, _) -> Right x

initAndRun :: ReaderT (MachineEnv ByteString) (EitherT ByteString IO) (ByteString, MachineState)
initAndRun = do
    mainAt <- findLabel "main"
    let state = MachineState { getIp      = mainAt
                             , getIpStack = []
                             , getStack   = []
                             , getRegs    = mempty }
    runStateT run state

run :: D m ByteString
run = do

    ci <- currentInstr

    case ci of

        DComment {} ->
            err [i|Undefined: #{ci}|]

        DLabel {} -> do
            next
            run

        DPush v -> do
            push v
            next
            run

        DPop r -> do
            pop r
            next
            run

        DCall dst -> do
            call dst
            run

        DRet v ->
            ret v >>= \case
                Left ()  -> run
                Right v' -> pure [i|Success: #{v'}|]

        DBin dst op a b-> do
            binOp dst op a b
            next
            run

        DNeg{} ->
            err [i|Undefined: #{ci}|]

        DCmpB {} ->
            err [i|Undefined: #{ci}|]

        J {} ->
            err [i|Undefined: #{ci}|]

        Jne {} ->
            err [i|Undefined: #{ci}|]

        DMov mode -> do
            move mode
            next
            run

err :: ByteString -> D m a
err msg = do
    env   <- lift ask
    state <- get
    let ip = getIp state
    case getInstrs env !? ip of
        Nothing -> lift . lift . left $ msg
        Just instr -> lift . lift . left $ [i|At instr #{ip}\n#{instr}\n#{msg}|]

next :: D m ()
next = modify $ \ms -> ms { getIp = getIp ms + 1 }

push :: DVal ByteString -> D m ()
push (DLitInt n) = modify $ \ms -> ms { getStack = n : getStack ms }
push v = err [i|Undefined push: #{v}|]

ret :: DVal ByteString -> D m (Either () Int)
ret (DReg r) = do   

    state <- get

    r' <- case M.lookup r (getRegs state) of
              Nothing -> err [i|Returning unknown reg #{r}|]
              Just r' -> pure r'

    case getIpStack state of
        []         -> pure $ Right r' -- Happy exit
        (ip:stack) -> do
            put state { getIp      = ip
                      , getIpStack = stack
                      , getStack   = r' : getStack state }
            pure $ Left () -- Program not finished - continue

ret v = err [i|Undefined ret: #{v}|]

move :: MovMode ByteString -> D m ()

move (ToFrom dst src) = do
    state <- get
    case M.lookup src (getRegs state) of
        Nothing   -> err [i|No such src reg: #{src}|]
        Just src' -> put state { getRegs = M.insert dst src' (getRegs state) }

move mode = err [i|Undefined move mode: #{mode}|]

pop :: R -> D m ()
pop r = do
    state <- get
    case getStack state of
        []       -> err [i|Tried to pop an empty stack into #{r}|]
        (s:tack) ->
            put state { getStack = tack
                      , getRegs  = M.insert r s (getRegs state) }

binOp :: R -> DBinOp -> DVal ByteString -> DVal ByteString -> D m ()
binOp dst DTimes a b = do
    a' <- asInt a
    b' <- asInt b
    modify $ \ms -> ms { getRegs = M.insert dst (a' * b') (getRegs ms) }

binOp dst DPlus a b = do
    a' <- asInt a
    b' <- asInt b
    modify $ \ms -> ms { getRegs = M.insert dst (a' + b') (getRegs ms) }

binOp   _ op _ _ = err [i|Undefined binop: #{op}|]

asInt :: DVal ByteString -> D m Int
asInt (DLitInt n) = pure n
asInt (DReg r) = do
    regs <- getRegs <$> get
    case M.lookup r regs of
        Nothing -> err [i|Register was not set: #{r}|]
        Just v -> pure v
asInt x = err [i|Undefined asInt: #{x}|]

call :: CallDest ByteString -> D m ()
call (CallLabel lbl) = do
    ip <- getIp <$> get
    let resumeIp = ip + 1
    x <- lift $ findLabel lbl
    modify $ \ms -> ms { getIp      = x
                       , getIpStack = resumeIp : getIpStack ms
                       }
call c = err [i|Undefined call: #{c}|]

findLabel :: ByteString -> ReaderT (MachineEnv ByteString) (EitherT ByteString IO) Int
findLabel lbl = do

    is <- getInstrs <$> ask

    case V.findIndex isLbl is of
            Just x  -> pure x
            Nothing -> lift $ left "No main found"

    where
    isLbl (DLabel ls) = lbl == ls
    isLbl           _ = False

currentInstr :: D m (DInstr ByteString)
currentInstr = do
    is <- getInstrs <$> lift ask
    ip <- getIp <$> get
    case is !? ip of
        Nothing -> lift . lift $ left "No such instruction!"
        Just x  -> pure x

newtype MachineEnv s =
    MachineEnv { getInstrs :: Vector (DInstr s) }

data MachineState =
    MachineState { getIp      :: !Int
                 , getIpStack :: ![Int]
                 , getStack   :: ![Int]
                 , getRegs    :: !(Map R Int)
                 }
