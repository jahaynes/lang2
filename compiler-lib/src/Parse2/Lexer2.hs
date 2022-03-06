{-# LANGUAGE OverloadedStrings #-}

module Parse2.Lexer2 where

import qualified Parse2.ByteString as B
import           Parse2.Parse2
import           Parse2.Token2

import           Data.ByteString         (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import           Data.Char               (isAlphaNum, isPunctuation, isSpace)
import           Data.List               (foldl')
import           Data.IntSet             (IntSet)
import qualified Data.IntSet as IS
import           Data.Word               (Word8)

runLexer :: ByteString -> [Pos Token]
runLexer source =
    let lineStarts = findLineStarts source
    in
    case runParser' (lexer lineStarts) source of
        Left e -> error $ show e
        Right (_, xs) -> xs

lexer :: IntSet -> Parser (Pos ByteString) [Pos Token]
lexer lineStarts = reverse . snd . foldl' insertBreaks (-1, []) <$> many nextToken
    where
    insertBreaks :: (Int, [Pos Token]) -> (Int, Pos Token) -> (Int, [Pos Token])
    insertBreaks (lastCol, acc) (col, t)
        | col > lastCol = (col, t:acc)
        | otherwise     = (col, t:Pos (Byte (-66)) TBreak:acc)

    nextToken :: Parser (Pos ByteString) (Int, Pos Token)
    nextToken = do
        t@(Pos (Byte b) _) <- B.dropWhile isSpace *> token
        let Just lineStart = IS.lookupLE b lineStarts
            col = b - lineStart
        pure (col, t)

token :: Parser (Pos ByteString) (Pos Token)
token = keyword
    <|> operator
    <|> litBool
    <|> litInt
    <|> litString
    <|> variable
    <|> constructor

    where
    keyword = positioned TLet  (B.string "let"  <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned TIn   (B.string "in"   <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned TIf   (B.string "if"   <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned TThen (B.string "then" <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned TElse (B.string "else" <* B.notFollowedBy alphaNumOrPunc)

    operator = positioned TEqEq   (B.string "==")
           <|> positioned TEq     (B.string "=")
           <|> positioned TPlus   (B.string "+")
           <|> positioned TMul    (B.string "*")
           <|> positioned TDiv    (B.string "/")
           <|> positioned TLambda (B.string "\\")
           <|> positioned TDot    (B.string ".")
           <|> positioned TLParen (B.string "(")
           <|> positioned TRParen (B.string ")")

    litBool = positioned (TLitBool True)  (B.string "True"  <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned (TLitBool False) (B.string "False" <* B.notFollowedBy alphaNumOrPunc)

    litInt = (\(Pos b i) -> Pos b (TLitInt i)) <$> B.integer -- TODO not followed by ., etc.

    variable = fmap TLowerStart <$> B.lowerStart

    constructor = fmap TUpperStart <$> B.upperStart

    alphaNumOrPunc c = isAlphaNum c || isPunctuation c

litString :: Parser (Pos ByteString) (Pos Token)
litString = Parser f
    where
    f p@(Pos by@(Byte b) s)
        | BS.null s       = errorAt "Out of litString" p
        | BS.head s /= 34 = errorAt "Doesn't start with \"" p
        | otherwise       =
            case findEnd 1 (BS.length s) False mempty of
                Left l         -> Left l
                Right (end, x) -> let s' = BS.drop end s
                                      b' = Byte (b + end + 1)
                                  in Right ( Pos b' (BS.tail s')
                                           , Pos by (TLitString x))

        where
        findEnd :: Int -> Int -> Bool -> Builder -> Either ByteString (Int, ByteString)
        findEnd i j esc acc
            | i == j    = Left "Ran off the end"
            | otherwise =
                let c = s `BS.index` i
                in
                if esc
                    then findEnd (i+1) j False (acc <> BB.word8 c)
                    else case c of
                             92 -> findEnd (i+1) j True acc
                             34 -> Right (i, LBS.toStrict $ BB.toLazyByteString acc)
                             _  -> findEnd (i+1) j False (acc <> BB.word8 c)

data LineState =
    LineState { newLines :: ![Int]
              , position :: !Int
              , last     :: !Word8 }

findLineStarts :: ByteString -> IntSet
findLineStarts bs
    | BS.null bs = mempty
    | otherwise  = IS.fromList . newLines . BS.foldl' f (LineState [0] 0 0) $ bs
    where
    f :: LineState -> Word8 -> LineState
    f (LineState nl p l) x =
        let nl' = case l of
                      10 -> p : nl
                      13 -> case x of
                                10 -> nl
                                _  -> p : nl
                      _  -> nl
        in LineState nl' (p+1) x