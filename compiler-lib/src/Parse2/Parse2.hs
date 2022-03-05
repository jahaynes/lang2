{-# LANGUAGE OverloadedStrings, InstanceSigs, ScopedTypeVariables #-}

module Parse2.Parse2 where

import           Parse2.Token2

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor                ((<&>))
import           Text.Printf                 (printf)

newtype Byte =
    Byte Int
        deriving (Read, Show)

class PosInfo a where
    byte :: a -> Byte

class Alternative f where
  (<|>) :: f a -> f a -> f a

newtype Parser s a =
    Parser { runParser :: s -> Either ByteString (s, a) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser run) = Parser $ \s -> (\(s',a) -> (s', f a)) <$> run s

instance Applicative (Parser s) where

    pure :: a -> Parser s a
    pure x = Parser $ \s -> Right (s, x)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> px = Parser $ \s ->
        case runParser pf s of
            Left l -> Left l
            Right (s', f) ->
                case runParser px s' of
                    Left l -> Left l
                    Right (ss', a) -> Right (ss', f a)

instance Monad (Parser s) where
    return :: a -> Parser s a
    return = pure

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    ma >>= f = Parser $ \s ->
        case runParser ma s of
            Left l -> Left l
            Right (s', a) -> runParser (f a) s'

instance Alternative (Parser s) where -- TODO accumulate failure alternatives?
    p1 <|> p2 = Parser $ \s ->
        case runParser p1 s of
            Right r -> Right r
            Left _  ->
                case runParser p2 s of
                    Right r -> Right r
                    Left _ -> Left "no alternatives left"

runParser' :: Parser (Pos s) a -> s -> Either ByteString (Pos s, a)
runParser' p s = runParser p (Pos (Byte 0) s)

data Pos a =
    Pos !Byte !a
        deriving (Read, Show)

instance PosInfo (Pos a) where
    byte (Pos b _) = b

instance Functor Pos where
    fmap f (Pos b a) = Pos b (f a)

positioned :: Functor f => Token -> f (Pos a) -> f (Pos Token)
positioned t parser = parser <&> \(Pos b _) -> Pos b t

errorAt :: PosInfo a => ByteString -> a -> Either ByteString r
errorAt msg x = 
    let Byte b = byte x
        at    = printf " at byte %d\n" b
    in Left (msg <> C8.pack at)

many :: Parser s a -> Parser s [a]
many p = Parser $
    let loop acc s =
          case runParser p s of 
            Left _        -> Right (s, reverse acc)
            Right (s', x) -> loop (x:acc) s'
    in loop []
