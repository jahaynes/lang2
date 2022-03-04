{-# LANGUAGE OverloadedStrings #-}

module Parse2.Lexer2 where

import qualified Parse2.ByteString as B
import           Parse2.Parse2
import           Parse2.Token2

import           Data.ByteString             (ByteString)
import qualified Data.ByteString       as BS
import           Data.Char                   (isAlphaNum, isPunctuation, isSpace)
import           Data.List                   (foldl')
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet as IS
import           Data.Word                   (Word8)

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

    where
    keyword = positioned TLet (B.string "let" <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned TIn  (B.string "in"  <* B.notFollowedBy alphaNumOrPunc)

    operator = positioned TEqEq (B.string "==")
           <|> positioned TEq   (B.string "=")
           <|> positioned TPlus (B.string "+")

    litBool = positioned (TLitBool True)  (B.string "True"  <* B.notFollowedBy alphaNumOrPunc)
          <|> positioned (TLitBool False) (B.string "False" <* B.notFollowedBy alphaNumOrPunc)

    litInt = (\(Pos b i) -> Pos b (TLitInt i)) <$> B.integer -- TODO not followed by ., etc.

    alphaNumOrPunc c = isAlphaNum c || isPunctuation c

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