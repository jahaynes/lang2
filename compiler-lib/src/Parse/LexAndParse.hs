{-# LANGUAGE OverloadedStrings #-}

module Parse.LexAndParse (findLineStarts, tokensToByteString) where

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

data LineState =
    LineState { newLines  :: ![Int]
              , _position :: !Int
              , _last     :: !Word8 }

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
