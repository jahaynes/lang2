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

        Push _ _ val -> do
            push val
            modifyIp (+1)
            step

        Pop _ _ dst -> do
            writeReg dst =<< pop
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
                Nothing -> do
                    pop >>= \case
                        i@VirtRegPrim{} -> do
                            x <- readReg i
                            left . pack . show $ x
                        RLitInt i     -> left . pack $ show i
                Just ip -> do
                    setIp ip
                    step

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

-- "write reg reg" ?
move :: SVal -> SVal -> Ma ()
move dst src = writeReg dst =<< readReg src

-- "write immediate" ?
writeReg :: SVal -> SVal -> Ma ()
writeReg dst@VirtRegPrim{} src = lift . modify' $ \ps -> ps { _registers = M.insert dst src (_registers ps) }

readReg :: SVal -> Ma SVal
readReg reg = do
    registers <- _registers <$> lift get
    case M.lookup reg registers of
        Nothing -> left "No such reg"
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

binOp :: SVal -> BinOp -> SVal -> SVal -> Ma ()
binOp dst op a b = do

    regs <- _registers <$> lift get

    va <- case M.lookup a regs of
              Nothing -> left "no such reg"
              Just (RLitInt va) -> pure va

    vb <- case M.lookup b regs of
              Nothing -> left "no such reg"
              Just (RLitInt vb) -> pure vb

    vc <- case op of
              MulI -> pure . RLitInt $! va * vb
              AddI -> pure . RLitInt $! va + vb
              _    -> left "unknown op"

    writeReg dst vc

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

getInstr :: Ma (AInstr ByteString)
getInstr = do
    ps <- lift get
    case I.lookup (_ip ps) (_program ps) of
        Nothing -> left "no such instruction"
        Just i  -> pure i
