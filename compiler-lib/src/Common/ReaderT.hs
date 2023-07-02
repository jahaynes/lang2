module Common.ReaderT where

import Common.Trans

import Data.Functor ((<&>))

newtype ReaderT s m a =
    ReaderT { runReaderT :: s -> m (a, s) }

instance Functor m => Functor (ReaderT s m) where

    fmap f (ReaderT rs) = ReaderT $ \s ->
        rs s <&> \(a, s') -> (f a, s')

instance Monad m => Applicative (ReaderT s m) where

    pure x = ReaderT $ \s -> pure (x, s)

    ReaderT rf <*> ReaderT rx = ReaderT $ \s -> do
        (f, s' ) <- rf s
        (x, s'') <- rx s'
        pure (f x, s'')

instance Monad m => Monad (ReaderT s m) where

    return = pure

    ReaderT rma >>= mf = ReaderT $ \s -> do
        (a, s') <- rma s
        runReaderT (mf a) s'

instance Trans (ReaderT s) where

    lift ma = ReaderT $ \s -> do
        a <- ma
        pure (a, s)

ask :: Applicative m => ReaderT s m s
ask = ReaderT $ \s -> pure (s, s)

evalReaderT :: Functor m => ReaderT s m a -> s -> m a
evalReaderT reader env = fst <$> runReaderT reader env
