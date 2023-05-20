module Common.Identity where

newtype Identity a =
    Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return = pure
    Identity a >>= f = f a