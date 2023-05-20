module Common.Trans where

class Trans t where
    lift :: Monad m => m a -> t m a
