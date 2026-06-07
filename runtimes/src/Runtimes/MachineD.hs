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
                             , getRegs    = mempty
                             , getCmp     = Nothing }
    runStateT (run 10000) state

run :: Int -> D m ByteString
run 0 = err [i|Out of instructions|]
run n = do

    ci <- currentInstr

    case ci of

        DComment {} ->
            err [i|Undefined: #{ci}|]

        DLabel {} -> do
            next
            run (n-1)

        DPush v -> do
            push v
            next
            run (n-1)

        DPop r -> do
            pop r
            next
            run (n-1)

        DCall dst -> do
            call dst
            run (n-1)

        DRet v ->
            ret v >>= \case
                Left ()  -> run (n-1)
                Right v' -> pure [i|Success: #{v'}|]

        DBin dst op a b-> do
            binOp dst op a b
            next
            run (n-1)

        DNeg{} ->
            err [i|Undefined: #{ci}|]

        DCmpB r -> do
            -- TODO - compare {x86, arm, webasm}.  Find the best way to do it in common
            --      - May need to go back to the generation / instruction set
            cmp r
            next
            run (n-1)

        J dst -> do
            jmp dst
            run (n-1)

        JF dst -> do
            jmpCmp (==False) dst
            run (n-1)

        DMov mode -> do
            move mode
            next
            run (n-1)

err :: ByteString -> D m a
err msg = do
    state <- get
    let ip = getIp state
    lift $ ask >>= \env ->
        lift $ case getInstrs env !? ip of
                   Nothing    -> left  msg
                   Just instr -> left [i|At instr #{ip}\n#{instr}\n#{msg}|]

next :: D m ()
next = modify $ \ms -> ms { getIp = getIp ms + 1 }

-- For now, just mv from reg to cmp here - it's already been translated
cmp :: R -> D m ()
cmp r = get >>= \state ->
    case M.lookup r (getRegs state) of
        Nothing -> err "No"
        Just 0  -> put state { getCmp = Just False }
        Just 1  -> put state { getCmp = Just True }
        Just _  -> err [i|bad bool|]

jmp :: ByteString -> D m ()
jmp dst = do
    state <- get
    dstAt <- lift $ findLabel dst
    lift . lift . lift $ putStrLn [i|Jumping to #{dst}/#{dstAt}|]
    put state { getIp = dstAt }

jmpCmp :: (Bool -> Bool) -> ByteString -> D m ()
jmpCmp pb dst = do
    state <- get
    case getCmp state of
        Nothing  -> err "jne called but CMP not set"
        Just b'
            | pb b' -> do
                dstAt <- lift $ findLabel dst
                lift . lift . lift $ putStrLn [i|Jumping to #{dst}/#{dstAt}|]
                put state { getIp = dstAt }
            | otherwise ->
                put state { getIp  = getIp state + 1 }

push :: DVal ByteString -> D m ()
push (DLitInt n) = modify $ \ms -> ms { getStack = n : getStack ms }
push (DReg r) = do
    regs <- getRegs <$> get
    case M.lookup r regs of
        Nothing -> err [i|Pushing unknown reg #{r}|]
        Just r' -> modify $ \ms -> ms { getStack = r' : getStack ms }
push v = err [i|Undefined push: #{v}|]

ret :: R -> D m (Either () Int)
ret r = do

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

move (FromLitInt dst src) =
    modify $ \s -> s { getRegs = M.insert dst src (getRegs s) }

move mode = err [i|Undefined move mode: #{mode}|]

pop :: R -> D m ()
pop r = do
    state <- get
    case getStack state of
        []       -> err [i|Tried to pop an empty stack into #{r}|]
        (s:tack) ->
            put state { getStack = tack
                      , getRegs  = M.insert r s (getRegs state) }

binOp :: R -> DBinOp -> R -> R -> D m ()
binOp dst DTimes a b = do
    a' <- asInt a
    b' <- asInt b
    modify $ \ms -> ms { getRegs = M.insert dst (a' * b') (getRegs ms) }

binOp dst DPlus a b = do
    a' <- asInt a
    b' <- asInt b
    modify $ \ms -> ms { getRegs = M.insert dst (a' + b') (getRegs ms) }

binOp dst DMinus a b = do
    a' <- asInt a
    b' <- asInt b
    modify $ \ms -> ms { getRegs = M.insert dst (a' - b') (getRegs ms) }

binOp dst DEq a b = do
    a' <- asInt a       -- TODO non-int
    b' <- asInt b       -- TODO non-int
    let c = if a' == b' then 1 else 0 -- Not suitable for non-jump-booling?
    modify $ \ms -> ms { getRegs = M.insert dst c (getRegs ms) }

binOp dst DLt a b = do
    a' <- asInt a       -- TODO non-int
    b' <- asInt b       -- TODO non-int
    let c = if a' < b' then 1 else 0 -- Not suitable for non-jump-booling?
    modify $ \ms -> ms { getRegs = M.insert dst c (getRegs ms) }

binOp   _ op _ _ = err [i|Undefined binop: #{op}|]

asInt :: R -> D m Int
-- asInt (DLitInt n) = pure n
asInt r = do
    regs <- getRegs <$> get
    case M.lookup r regs of
        Nothing -> err [i|Register was not set: #{r}|]
        Just v -> pure v
--- asInt x = err [i|Undefined asInt: #{x}|]

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
                 , getCmp     :: !(Maybe Bool)
                 }

{-
data DCmp = DcFalse
          | DcTrue
          | DcEq
          | DcNeq
          | DcGt
          | DcGEq
          | DcLt
          | DcLEq
              deriving Show

cmpCode :: DCmp -> Int
cmpCode DcFalse = 0
cmpCode DcTrue  = 1
cmpCode DcEq    = 2
cmpCode DcNeq   = 3
cmpCode DcGt    = 4
cmpCode DcGEq   = 5
cmpCode DcLt    = 6
cmpCode DcLEq   = 7

unCmpCode :: Int -> DCmp
unCmpCode 0 = DcFalse
unCmpCode 1 = DcTrue
unCmpCode 2 = DcEq
unCmpCode 3 = DcNeq
unCmpCode 4 = DcGt
unCmpCode 5 = DcGEq
unCmpCode 6 = DcLt
unCmpCode 7 = DcLEq
-}
