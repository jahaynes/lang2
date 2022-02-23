module Common.State where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f state = State $ \s ->
        let (a, s') = runState state s
        in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    State rf <*> State rx = State $ \s ->
        let (f, s') = rf s
            (x, s'') = rx s'
        in (f x, s'')

instance Monad (State s) where
    return = pure
    State rx >>= f = State $ \s ->
        let (sx, s') = rx s
            State ry = f sx
        in ry s'

get :: State x x
get = State $ \s -> (s, s)

put :: x -> State x ()
put x = State $ const ((), x)

getThenApply :: (x-> x) -> State x x
getThenApply f = get >>= \x -> put (f x) >> pure x

modify' :: (x -> x) -> State x ()
modify' f = State $ \x ->
  let y = f x
  in
  case y of
    _ -> ((), y)
