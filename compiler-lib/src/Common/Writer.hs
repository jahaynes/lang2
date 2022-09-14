module Common.Writer (Writer, runWriter, write, getLog) where

newtype Writer w a = Writer { runWriter' :: [w] -> (a, [w]) }

instance Functor (Writer s) where
    fmap f writer = Writer $ \w ->
        let (a, w') = runWriter' writer w
        in (f a, w')

instance Applicative (Writer s) where
    pure a = Writer $ \w -> (a, w)
    Writer rf <*> Writer rw = Writer $ \w ->
        let (f, w') = rf w
            (x, w'') = rw w'
        in (f x, w'')

instance Monad (Writer s) where
    return = pure
    Writer rw >>= f = Writer $ \w ->
        let (a, w') = rw w
            Writer b = f a
        in b w'

write :: w -> Writer w ()
write x = Writer $ \w -> ((), x:w)

getLog :: Writer w [w]
getLog = Writer $ \w -> (w, w)

runWriter :: [w] -> Writer w a -> (a, [w])
runWriter ws' writer =
    let (a, ws) = runWriter' writer ws'
    in (a, reverse ws)
