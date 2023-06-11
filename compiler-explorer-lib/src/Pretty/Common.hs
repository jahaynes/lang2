module Pretty.Common where

import           Data.ByteString    (ByteString)
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
