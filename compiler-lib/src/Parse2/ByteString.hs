{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Parse2.ByteString where

import           Parse2.Parse2

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char                   (isDigit)

eof :: Parser (Pos ByteString) ()
eof = Parser $ \ps@(Pos _ s) ->
    if C8.null s
        then Right (ps, ())
        else errorAt "expected EOF" ps

string :: ByteString -> Parser (Pos ByteString) (Pos ByteString)
string bs = Parser $ \ps@(Pos by@(Byte b) s) ->
    let len          = C8.length bs
        (some, rest) = C8.splitAt len s
    in
    if C8.length some < len
        then errorAt ("out of input for: " <> bs) ps
        else if bs == some
            then Right (Pos (Byte $ b + len) rest, Pos by some)
            else errorAt ("expected: " <> bs) ps

digits :: Parser (Pos ByteString) (Pos ByteString)
digits = Parser $ \ps@(Pos by@(Byte b) s) ->
    let ds = C8.takeWhile isDigit s
    in if C8.null ds
           then errorAt ("Expected digits") ps
           else let len = C8.length ds
                    b'  = Byte (b + len)
                    s'  = C8.drop len s
                in Right (Pos b' s', Pos by ds)

integer :: Parser (Pos ByteString) (Pos Integer)
integer = fmap (read . C8.unpack) <$> digits

takeWhile :: (Char -> Bool) -> Parser (Pos ByteString) (Pos ByteString)
takeWhile p = Parser $ \(Pos by@(Byte b) s) ->
    let a   = C8.takeWhile p s
        len = C8.length a
        b'  = Byte (b + len)
        s'  = C8.drop len s
    in Right (Pos b' s', Pos by a)

dropWhile :: (Char -> Bool) -> Parser (Pos ByteString) ()
dropWhile p = Parser $ \(Pos (Byte b) s) ->
    let (dropped, kept) = C8.span p s
        b' = Byte (b + C8.length dropped)
    in Right (Pos b' kept, ())

notFollowedBy :: (Char -> Bool) -> Parser (Pos ByteString) ()
notFollowedBy p = Parser $ \pos@(Pos _ s) ->
  if C8.null s
     then pure (pos, ())
     else if p (C8.head s)
              then errorAt "was followed by predicate" pos
              else pure (pos, ())