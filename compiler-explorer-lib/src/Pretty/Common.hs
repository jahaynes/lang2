module Pretty.Common where

import           Text.Builder       (Builder)
import qualified Text.Builder as TB

data Atomic = Atom | Group

group :: (Atomic, Builder) -> Builder
group (Atom, b)  = b
group (Group, b) = mconcat [TB.char '(', b, TB.char ')']