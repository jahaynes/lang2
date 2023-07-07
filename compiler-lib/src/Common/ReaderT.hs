module Common.ReaderT where

import Common.Trans

newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where

    fmap f (ReaderT run) =
        ReaderT $ fmap f . run

instance Applicative m => Applicative (ReaderT r m) where

    pure x = ReaderT $ \_ ->
        pure x

    ReaderT runF <*> ReaderT runX = ReaderT $ \r ->
        runF r <*> runX r

instance Monad m => Monad (ReaderT r m) where

    return = pure

    ma >>= b = ReaderT $ \r -> do
        a <- runReaderT ma r
        runReaderT (b a) r

instance Trans (ReaderT r) where

    lift m = ReaderT $ \_ -> m

ask :: Applicative m => ReaderT r m r
ask = ReaderT pure
