module Common.EitherT ( EitherT(..)
                      , left
                      ) where

import Common.Trans

newtype EitherT l m r = EitherT { runEitherT :: m (Either l r) }

left :: Monad m => l -> EitherT l m a
left = EitherT . pure . Left

instance Functor m => Functor (EitherT l m) where

    fmap f (EitherT ret) = EitherT (fmap (fmap f) ret)

instance Monad m => Applicative (EitherT l m) where

    pure = EitherT . pure . Right

    EitherT retf <*> EitherT retx = EitherT $ do
        ef <- retf
        case ef of
            Left l  -> pure $ Left l
            Right f -> do
                ex <- retx
                case ex of
                    Left l  -> pure $ Left l
                    Right x -> pure . Right $ f x

instance Monad m => Monad (EitherT l m) where

    return = pure

    EitherT rma >>= mf = EitherT $ do
        ma <- rma
        case ma of
            Left l  -> pure (Left l)
            Right r -> runEitherT (mf r)

instance Trans (EitherT l) where

    lift ma = EitherT $ Right <$> ma
