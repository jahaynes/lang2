module Common.EitherT where

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT run) =
        EitherT $ fmap (fmap f) run

instance Applicative m => Applicative (EitherT e m) where

    pure =
        EitherT . pure . Right

    EitherT mf <*> EitherT mx =
        EitherT $ (\ef ex -> ($) <$> ef <*> ex) <$> mf <*> mx

instance Monad m => Monad (EitherT e m) where

    return = pure

    EitherT ma >>= f = EitherT $ do
        a <- ma
        case a of
            Left l -> pure $ Left l
            Right r ->
                let EitherT y = f r
                in y

left :: Applicative m => e -> EitherT e m a
left = EitherT . pure . Left
