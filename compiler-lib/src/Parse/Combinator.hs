module Parse.Combinator where

import Parse.LexState
import Parse.Parser

import           Data.ByteString       (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe            (fromMaybe)
import           Data.Word             (Word8)

manyIncreasinglyRight :: (Eq s, Show s)
                      => Maybe Int
                      -> Parser [(SourcePos, s)] (SourcePos, a)
                      -> Parser [(SourcePos, s)] [(SourcePos, a)]
manyIncreasinglyRight mCol p = Parser $
  let loop acc col s =
        case runParser p s of
          Right (s', r@(SourcePos _ col', _))
              | col' >= col -> loop (r:acc) col' s'
              | otherwise   -> Right (s, reverse acc)
          Left _            -> Right (s, reverse acc)
  in loop [] (fromMaybe (-1) mCol)

many :: Parser s a -> Parser s [a]
many p = Parser $
    let loop acc s =
          case runParser p s of 
            Left _        -> Right (s, reverse acc)
            Right (s', x) -> loop (x:acc) s'
    in loop []

many1 :: Parser s a -> Parser s [a]
many1 p = do
    xs <- many p
    if null xs
      then Parser $ \_ -> Left "Could not find 1 match"
      else pure xs

dropWhile' :: (Word8 -> Bool) -> Parser LexState ()
dropWhile' p = Parser $ \ls@(LexState _ s) ->
  let toDrop = BS.takeWhile p s
  in Right (consumedBs ls toDrop, ())

takeWhile' :: (Word8 -> Bool) -> Parser LexState ByteString
takeWhile' p = Parser $ \ls@(LexState _ s) ->
  let some = BS.takeWhile p s
  in Right (consumedBs ls some, some)
