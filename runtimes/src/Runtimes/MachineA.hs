{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Runtimes.MachineA (runMachineA) where

import Common.EitherT
import Common.State
import Common.Trans
import Core.Operator
import Phase.CodeGen.TypesA

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           Data.Map (Map)
import qualified Data.Map as M

data AState s =
    AState { _program   :: !(IntMap (AInstr s))
           , _stack     :: ![SVal]
           , _ip        :: !Int
           , _ipStack   :: ![Int]
           , _registers :: !(Map SVal SVal)
           , _cmp       :: !Bool
           }

type Ma a =
    EitherT ByteString (State (AState ByteString)) a

runMachineA :: [AInstr ByteString] -> ByteString
runMachineA instrs = do

    let state = AState { _program   = I.fromList (zip [0..] instrs)
                       , _stack     = []
                       , _ip        = 0
                       , _ipStack   = []
                       , _registers = mempty
                       , _cmp       = True
                       }

    case evalState (runEitherT run) state of
        Left l  -> l
        Right r -> r

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

        ABinOp dst op a b -> do
            binOp dst op a b
            modifyIp (+1)
            step

        ACmp a -> do
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
                            left . pack . show $ x
                        RLitInt i     -> left . pack $ show i
                Just ip -> do
                    setIp ip
                    step

        J lbl -> do
            j lbl
            step

        Jne lbl -> do
            jne lbl
            step

        inst -> left $ "Unkn instr: " <> pack (show inst)

resolveLabel :: ByteString -> Ma Int
resolveLabel lbl =
    (map fst . filter (\(_, x) -> x == ALabel lbl) . I.toList . _program <$> lift get) >>= \case
        []  -> left "No such label"
        [x] -> pure x
        _   -> left "Too many labels"

modifyIp :: (Int -> Int) -> Ma ()
modifyIp f = lift . modify' $ \ps -> ps { _ip = f $ _ip ps }

setIp :: Int -> Ma ()
setIp ip = lift . modify' $ \ps -> ps { _ip = ip }

getIp :: Ma Int
getIp = _ip <$> lift get

move :: SVal -> SVal -> Ma ()
move dst@VirtRegPrim{} src = writeReg =<< evalOnce src

    where
    writeReg :: SVal -> Ma ()
    writeReg src' = lift . modify' $ \ps -> ps { _registers = M.insert dst src' (_registers ps) }

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
push val@VirtRegPrim{} = readReg val >>= push
push _                 = left "push"

pop :: Ma SVal
pop = do
    stack <- _stack <$> lift get
    case stack of
        [] -> left "empty stack"
        (s:tack) -> lift $ do
            modify' $ \ps -> ps { _stack = tack }
            pure s

j :: ByteString -> Ma ()
j lbl = do
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
