{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer where

import           Parse.Parser
import           Parse.Token

import           Data.ByteString       (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Char             (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import           Data.Vector           (Vector)
import qualified Data.Vector as V


-- TODO applicative
lex'' :: ByteString -> Either ByteString (Vector Int, Vector Token)
lex'' s = lex' s $ do
    (positions, tokens) <- V.unzip <$> many nextPositionedToken
    pDropWhile isSpace
    pure (positions, V.fromList . disambiguateNegation $ V.toList tokens)

-- Split '-' into binary minus and unary negation, based on its preceding token
-- TODO vectorise
disambiguateNegation :: [Token] -> [Token]
disambiguateNegation = go [] TAmbiguous
    where
    go acc    _          [] = reverse acc
    go acc prev (TMinus:ts) =
        let x = f prev
        in go (x:acc) x ts
    go acc _ (t:ts) = go (t:acc) t ts

    f TIn     = TNegate
    f TIf     = TNegate
    f TThen   = TNegate
    f TElse   = TNegate
    f TDot    = TNegate
    f TLParen = TNegate
    f TEqEq   = TNegate
    f TGt     = TNegate
    f TGtEq   = TNegate
    f TLt     = TNegate
    f TLtEq   = TNegate
    f TEq     = TNegate
    f TPlus   = TNegate
    f TMinus  = TNegate
    f TMul    = TNegate
    f TDiv    = TNegate
    f TNegate = TNegate
    f TAnd    = TNegate
    f TOr     = TNegate

    f TRParen         = TMinus
    f (TLitInt _)     = TMinus
    f (TLowerStart _) = TMinus

    f TLet            = TAmbiguous
    f TLambda         = TAmbiguous
    f (TLitBool _)    = TAmbiguous
    f (TLitString _)  = TAmbiguous
    f (TUpperStart _) = TAmbiguous
    f TPipe           = TAmbiguous
    f TColon          = TAmbiguous
    f TArr            = TAmbiguous
    f TAmbiguous      = TAmbiguous
    f TPlusPlus       = TAmbiguous
    f TDollar         = TAmbiguous -- check
    f TCase           = TAmbiguous -- check
    f TOf             = TAmbiguous -- check
    f TErr            = TAmbiguous -- check

lex' :: ByteString -> Parser LexState a -> Either ByteString a
lex' s p =
    let ps = LexState { ls_source = s
                      , ls_pos    = 0
                      }
    in
    case runParser p ps of
        Left l -> Left l
        Right (s', x) | ls_pos s' == BS.length (ls_source s') -> Right x
                      | otherwise                             -> Left $ "Leftover input: " <> C8.pack (show s')

data LexState =
    LexState { ls_source     :: !ByteString
             , ls_pos        :: !Int
             } deriving Show

nextPositionedToken :: Parser LexState (Int, Token)
nextPositionedToken = do
    pDropWhile isSpace
    pos <- getPosition
    t   <- parseToken
    pure (pos, t)

pDropWhile :: (Char -> Bool) -> Parser LexState ()
pDropWhile p = Parser $ \ls -> f ls (ls_pos ls)
    where
    f ls pos
        | pos >= BS.length (ls_source ls) = Right (ls {ls_pos = pos}, ())
        | p (C8.index (ls_source ls) pos) = f ls (pos + 1)
        | otherwise                       = Right (ls {ls_pos = pos}, ())

parseToken :: Parser LexState Token
parseToken = keyword
         <|> operator
         <|> litBool
         <|> litInt
         <|> (TLitString <$> litString)
         <|> variable
         <|> constructor

    where
    keyword :: Parser LexState Token
    keyword = positioned TLet  (string "let"   <* notFollowedBy isAlphaNum)
          <|> positioned TIn   (string "in"    <* notFollowedBy isAlphaNum)
          <|> positioned TIf   (string "if"    <* notFollowedBy isAlphaNum)
          <|> positioned TThen (string "then"  <* notFollowedBy isAlphaNum)
          <|> positioned TElse (string "else"  <* notFollowedBy isAlphaNum)
          <|> positioned TErr  (string "error" <* notFollowedBy isAlphaNum)
          <|> positioned TCase (string "case"  <* notFollowedBy isAlphaNum)
          <|> positioned TOf   (string "of"    <* notFollowedBy isAlphaNum)

    operator :: Parser LexState Token
    operator = positioned TEqEq     (string "==")
           <|> positioned TGtEq     (string ">=")
           <|> positioned TGt       (string ">")
           <|> positioned TLtEq     (string "<=")
           <|> positioned TLt       (string "<")
           <|> positioned TEq       (string "=")
           <|> positioned TColon    (string ":")
           <|> positioned TPlusPlus (string "++")
           <|> positioned TPlus     (string "+")
           <|> positioned TArr      (string "->")
           <|> positioned TMinus    (string "-")
           <|> positioned TDollar   (string "$")
           <|> positioned TMul      (string "*")
           <|> positioned TDiv      (string "/")
           <|> positioned TLambda   (string "\\")
           <|> positioned TDot      (string ".")
           <|> positioned TLParen   (string "(")
           <|> positioned TRParen   (string ")")
           <|> positioned TAnd      (string "&&")
           <|> positioned TOr       (string "||")
           <|> positioned TPipe     (string "|")

    litBool = TLitBool <$> boolean

    litInt :: Parser LexState Token
    litInt = TLitInt <$> integer -- TODO not followed by ., etc.

    variable :: Parser LexState Token
    variable = TLowerStart <$> lowerStart

    constructor :: Parser LexState Token
    constructor = TUpperStart <$> upperStart

positioned :: Functor f => b -> f a -> f b
positioned t s = t <$ s

getPosition :: Parser LexState Int
getPosition = Parser $ \ps -> Right (ps, ls_pos ps)

boolean :: Parser LexState Bool
boolean = positioned True  (string "True"  <* notFollowedBy isAlphaNum)
      <|> positioned False (string "False" <* notFollowedBy isAlphaNum)

string :: ByteString -> Parser LexState ()
string bs = Parser $ \ps ->
    let len  = BS.length bs
        pos' = ls_pos ps + len
        some = BS.take len . BS.drop (ls_pos ps) . ls_source $ ps
    in
    if pos' > BS.length (ls_source ps)
        then Left "Insufficient input"
        else if bs == some
                 then Right (ps { ls_pos = pos' }, ())
                 else Left "String mismatch"

lowerStart :: Parser LexState ByteString
lowerStart = alphaNumStartWith isLower

integer :: Parser LexState Integer
integer = read . C8.unpack <$> digits

digits :: Parser LexState ByteString
digits = Parser $ \ls ->
    let ds = C8.takeWhile isDigit . C8.drop (ls_pos ls) $ ls_source ls
        len = C8.length ds
    in
    if C8.null ds
        then Left "Expected digits"
        else Right (ls { ls_pos = ls_pos ls + len }, ds)

upperStart :: Parser LexState ByteString
upperStart = alphaNumStartWith isUpper

alphaNumStartWith :: (Char -> Bool) -> Parser LexState ByteString
alphaNumStartWith p = Parser $ \ls ->
    let some = C8.takeWhile isAlphaNum . BS.drop (ls_pos ls) $ ls_source ls
        len  = C8.length some
    in if len == 0
           then Left "Expected alphaNum for alphaNumStartWith"
           else
               if p (C8.head some)
                   then Right (ls { ls_pos = ls_pos ls + len }, some)
                   else Left "Unexpected alphaNumStartWith"

notFollowedBy :: (Char -> Bool) -> Parser LexState ()
notFollowedBy p = Parser $ \ls ->
    let source = ls_source ls
        pos    = ls_pos ls
    in
    if pos >= BS.length source
        then pure (ls, ())
        else if p (C8.index source pos)
                 then Left "was followed by predicate"
                 else pure (ls, ())

litString :: Parser LexState ByteString
litString = Parser f
    where
    f (LexState source pos)

        | pos >= BS.length source =
            Left "Out of litString"

        | BS.index source pos /= 34 =
            Left "Doesn't start with \""

        | otherwise =
            case findEnd (pos + 1) (BS.length source) False of

                Left l -> Left l

                Right i -> let len = i - pos
                           in
                           Right ( LexState source (pos + len + 1)
                                 , BS.take (len - 1) $ BS.drop (pos + 1) source )

        where
        findEnd :: Int -> Int -> Bool -> Either ByteString Int
        findEnd i j esc
            | i == j    = Left "Ran off the end"
            | esc       = findEnd (i+1) j False
            | otherwise =
                case BS.index source i of
                    92 -> findEnd (i+1) j True
                    34 -> Right i
                    _  -> findEnd (i+1) j False
