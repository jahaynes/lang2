{-# LANGUAGE OverloadedStrings #-}

module Pretty.Common where

import           Data.ByteString    (ByteString)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           TextBuilder        (TextBuilder)
import qualified TextBuilder as TB

data Grouping = Atom | Paren | Braces

group :: (Grouping, TextBuilder) -> TextBuilder
group (Atom, b)  = b
group (Paren, b) = mconcat [TB.char '(', b, TB.char ')']
group (Braces, b) = mconcat [TB.char '{', b, TB.char '}']

bytestring :: ByteString -> TextBuilder
bytestring = TB.text . decodeUtf8

printVars :: [ByteString] -> TextBuilder
printVars = TB.intercalate " " . map bytestring

indent :: Int -> TextBuilder
indent i = TB.text $ T.replicate (2*i) " "
