{-# LANGUAGE OverloadedStrings #-}

module Pretty.Common where

import           Data.ByteString    (ByteString)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

data Grouping = Atom | Paren | Braces

group :: (Grouping, Builder) -> Builder
group (Atom, b)  = b
group (Paren, b) = mconcat [TB.char '(', b, TB.char ')']
group (Braces, b) = mconcat [TB.char '{', b, TB.char '}']

bytestring :: ByteString -> Builder
bytestring = TB.text . decodeUtf8

printVars :: [ByteString] -> Builder
printVars = TB.intercalate " " . map bytestring

indent :: Int -> Builder
indent i = TB.text $ T.replicate (2*i) " "
