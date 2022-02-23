{-# LANGUAGE OverloadedStrings #-}

module Parse.Primitives where

import Parse.Combinator
import Parse.LexState
import Parse.Parser

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char                   (isDigit, isAlpha, isAlphaNum)
import           Text.Read                   (readMaybe)

eof :: Parser LexState ()
eof = Parser $ \ls@(LexState _ s) ->
  if C8.null s
    then pure (ls, ())
    else Left $ "expected eof: was" <> C8.unpack s

char :: Char -> Parser LexState Char
char '\n' = error "newline char disallowed"
char '\r' = error "newline char disallowed"
char c = Parser $ \ls@(LexState _ s) ->
  if C8.null s
    then Left "Ran out"
    else if C8.head s == c
      then pure (consumedNonNewlineChar ls, c)
      else Left "Expected char"

string :: ByteString -> Parser LexState ByteString
string bs = Parser $ \ls@(LexState _ s) -> do
  let len = C8.length bs
  if C8.length s < len
    then Left "Ran out"
    else if bs == C8.take len s
      then Right (consumedBs ls bs, bs)
      else Left "Expected string"

skipSpaces :: Parser LexState ()
skipSpaces = dropWhile' isSpace
    where
    isSpace  9 = True
    isSpace 10 = True
    isSpace 13 = True
    isSpace 32 = True
    isSpace  _ = False

restOfString :: Parser LexState ByteString
restOfString = takeWhile' (/= 34) <* char '\"'   --TODO avoid escaped strings

restOfLine :: Parser LexState ByteString
restOfLine = skipSpaces *> takeWhile' (not . isEol) <* takeWhile' isEol
    where
    isEol 10 = True
    isEol 13 = True
    isEol  _ = False

digit :: Parser LexState Char
digit = Parser $ \ls@(LexState  _ s) ->
  if C8.null s
    then Left "Ran out"
    else if isDigit $ C8.head s
      then pure (consumedNonNewlineChar ls, C8.head s)
      else Left "Expected char"

int :: Parser LexState Integer
int = do
  strDigits <- takeWhile' isDigit'
  case readMaybe (C8.unpack strDigits) of
    Just digits -> alreadyConsumed digits
    Nothing     -> Parser $ \_ -> Left "Not digits"
  where
  isDigit' b = b >= 48 && b <= 57

bool :: Parser LexState Bool
bool = (True  <$ string "True")
   <|> (False <$ string "False")

alpha :: Parser LexState Char
alpha = Parser $ \ls@(LexState _ s) ->
  if C8.null s
    then Left "Ran out"
    else if isAlpha (C8.head s)
           then pure (consumedNonNewlineChar ls, C8.head s)
           else Left "Expected char"

alphaNum :: Parser LexState Char
alphaNum = Parser $ \ls@(LexState _ s) ->
  if C8.null s
    then Left "Ran out"
    else if isAlphaNum (C8.head s)
           then pure (consumedNonNewlineChar ls, C8.head s)
           else Left "Expected char"

notFollowedBy :: (Char -> Bool) -> Parser LexState ()
notFollowedBy p = Parser $ \ls@(LexState _ s) ->
  if C8.null s
     then pure (ls, ())
     else if p (C8.head s)
              then Left "was followed by predicate"
              else pure (ls, ())

