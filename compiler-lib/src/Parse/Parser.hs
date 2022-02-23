module Parse.Parser where

class Alternative f where
  (<|>) :: f a -> f a -> f a

newtype Parser s a = Parser { runParser :: s -> Either String (s, a) }

instance Functor (Parser s) where
  fmap f (Parser r) = Parser $ \s ->
    case r s of
      Left          l -> Left l
      Right (rest, x) -> Right (rest, f x)

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Right (s, x)
  Parser rf <*> Parser rx = Parser $ \s ->
    case rf s of
      Left l -> Left l
      Right (s', f) ->
        case rx s' of
          Left l -> Left l
          Right (s'', x) ->
            Right (s'', f x)

instance Monad (Parser s) where
  return = pure
  Parser rx >>= f = Parser $ \s ->
    case rx s of
      Left l -> Left l
      Right (s', x) ->
        let Parser r = f x
        in r s'

instance Alternative (Parser s) where
  Parser ra <|> Parser rb = Parser $ \s ->
    case ra s of
      Right a -> Right a
      Left _ -> rb s
