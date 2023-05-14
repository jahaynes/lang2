module Common.StateT where

import Control.Monad.IO.Class
import Data.Functor ((<&>))

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where

    fmap f (StateT rs) = StateT $ \s ->
        rs s <&> \(a, s') -> (f a, s')

instance Monad m => Applicative (StateT s m) where

    pure x = StateT $ \s -> pure (x, s)

    StateT rf <*> StateT rx = StateT $ \s -> do
        (f, s' ) <- rf s
        (x, s'') <- rx s'
        pure (f x, s'')

instance Monad m => Monad (StateT s m) where

    return = pure

    StateT rma >>= mf = StateT $ \s -> do
        (a, s') <- rma s
        runStateT (mf a) s'

instance MonadIO m => MonadIO (StateT s m) where

    liftIO ioa = StateT $ \s -> do
        a <- liftIO ioa
        pure (a, s)

gett :: Applicative m => StateT s m s
gett = StateT $ \s -> pure (s, s)

putt :: Applicative m => s -> StateT s m ()
putt x = StateT $ const $ pure ((), x)

modifyt :: Monad m => (s -> s) -> StateT s m ()
modifyt f = do
    s <- gett
    putt $! f s