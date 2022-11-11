module Phase.CodeGen.TagInfo where

import Core.Module
import Core.Types
import Data.List         (findIndex)

newtype Tag =
    Tag Int

-- TODO totality
getTag :: Eq s => [DataDefn s] -> Type s -> s -> Tag
getTag dataDefns (TyCon typ) name =
    let [DataDefn _ _ cs] = filter (\(DataDefn n _ _) -> n == typ) dataDefns
        Just i = findIndex (\(DataCon n _) -> n == name) cs
    in Tag i
