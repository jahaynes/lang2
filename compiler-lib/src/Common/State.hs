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

instance MonadFail (State s) where
    fail = error

get :: State x x
get = State $ \s -> (s, s)

put :: x -> State x ()
put x = State $ const ((), x)

modify' :: (x -> x) -> State x ()
modify' f = State $ \x ->
  let y = f x
  in
  case y of
    _ -> ((), y)

evalState :: State s a -> s -> a
evalState x = fst . runState x

evalState' :: s -> State s a -> a
evalState' = flip evalState

execState :: State s a -> s -> s
execState x = snd . runState x