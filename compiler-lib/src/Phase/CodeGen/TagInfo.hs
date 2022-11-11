module Phase.CodeGen.TagInfo where

import Core.Module
import Core.Types
import Phase.CodeGen.Val

newtype Tag =
    Tag Int

class Tagged a where
    getTag :: a -> Tag

newtype TaggedConsName s =
    TaggedConsName s

instance Tagged (TaggedConsName s) where
    getTag (TaggedConsName name) = Tag (-9)