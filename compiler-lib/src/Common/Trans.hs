module Common.Trans ( Trans (lift) ) where

class Trans t where
    lift :: Monad m => m a -> t m a
