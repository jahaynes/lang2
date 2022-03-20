{-# LANGUAGE OverloadedStrings #-}

module Parse.LexAndParse where

import Parse.Definition
import Parse.Lexer
import Parse.Parser

import           Data.ByteString             (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet as IS
import           Data.List                   (intercalate)
import           Data.Vector                 (toList)
import           Data.Word                   (Word8)

lexAndParse :: ByteString -> (ByteString, ByteString)
lexAndParse source =
    let lineStarts = findLineStarts source
    in
    case lex'' source of
        Left e -> (e, "")
        Right (pos, tokens) ->
            let strTokens = C8.unlines . map (C8.pack . show) $ toList tokens
            in
            case parse' pos lineStarts tokens parseDefns of
                Left e -> (strTokens, e)
                Right defns ->
                    let strDefns = C8.pack . intercalate "\n\n" $ map show defns
                    in
                    (strTokens, strDefns)

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