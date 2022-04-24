{-# LANGUAGE OverloadedStrings #-}

module Parse.LexAndParse where

import Core.Definition
import Parse.Definition
import Parse.Lexer
import Parse.Parser
import Parse.Token

import           Data.ByteString             (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet as IS
import           Data.Vector                 (Vector, toList)
import           Data.Word                   (Word8)

tokensToByteString :: Vector Token -> ByteString
tokensToByteString = C8.unlines . map (C8.pack . show) . toList

lexAndParse :: ByteString -> ( Either ByteString (Vector Token)
                             , Either ByteString (Module ByteString) )
lexAndParse source =
    case lex'' source of

        Left e -> ( Left e
                  , Left "" )

        Right (pos, tokens) -> ( Right tokens
                               , parse' pos (findLineStarts source) tokens parseDefns )

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