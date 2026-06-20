module Common.StateT ( StateT (..)
                     , evalStateT
                     , evalStateT'
                     , get
                     , modify
                     , put
                     ) where

import Common.Trans

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

instance Trans (StateT s) where

    lift ma = StateT $ \s -> do
        a <- ma
        pure (a, s)

get :: Applicative m => StateT s m s
get = StateT $ \s -> pure (s, s)

put :: Applicative m => s -> StateT s m ()
put x = StateT $ const $ pure ((), x)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = do
    s <- get
    put $! f s

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT st x = fst <$> runStateT st x

evalStateT' :: Functor m => s -> StateT s m a -> m a
evalStateT' = flip evalStateT
