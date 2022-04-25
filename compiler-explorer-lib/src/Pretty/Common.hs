module Pretty.Common where

import           Text.Builder       (Builder)
import qualified Text.Builder as TB

data Grouping = Atom | Paren | Braces

group :: (Grouping, Builder) -> Builder
group (Atom, b)  = b
group (Paren, b) = mconcat [TB.char '(', b, TB.char ')']
group (Braces, b) = mconcat [TB.char '{', b, TB.char '}']