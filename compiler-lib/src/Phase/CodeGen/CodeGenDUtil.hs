module Phase.CodeGen.CodeGenDUtil where

import Common.EitherT             (EitherT, left)
import Common.ReaderT             (ReaderT)
import Common.StateT              (StateT, get, modify, put)
import Common.Trans               (lift)
import Phase.CodeGen.TypesD       (R (R))

import           Data.ByteString.Char8 (ByteString)
import           Data.Map              (Map)
import qualified Data.Map.Strict as M

type CgT m e s a =
    EitherT e (
        ReaderT (Env ()) (
            StateT (St s) m)) a

newtype Env s =
    Env s

data St s =
    St { nextNum      :: !Int
       , varRegisters :: !(Map s R)
       }

type CgM m a =
    CgT m ByteString ByteString a

saveRegisterMap :: Monad m => CgM m (Map ByteString R)
saveRegisterMap = lift $ lift (varRegisters <$> get)

restoreRegisterMap :: Monad m => Map ByteString R -> CgM m ()
restoreRegisterMap regMap = lift . lift . modify $ \s -> s { varRegisters = regMap }

getRegister :: Monad m => ByteString -> CgM m (Maybe R)
getRegister v = lift $ do
    vr <- varRegisters <$> lift get
    pure $ M.lookup v vr

bindFreshReg :: Monad m => ByteString -> CgM m R
bindFreshReg v = do
    fresh <- freshReg
    register v fresh
    pure fresh

register :: Monad m => ByteString -> R -> CgM m ()
register var reg = lift . lift . modify $ \s -> s { varRegisters = M.insert var reg (varRegisters s) }

freshReg :: Monad m => CgM m R
freshReg = R <$> freshNum

freshNum :: Monad m => CgM m Int
freshNum = lift . lift $ do
    gen <- get
    let rc = nextNum gen
    put gen { nextNum = rc + 1 }
    pure rc

err :: Monad m => ByteString -> CgM m a
err msg = left msg
