module Phase.CodeGen.TagInfo where

import Common.EitherT
import Common.ReaderT
import Common.Trans
import Core.Module
import Core.Types

import Data.ByteString   (ByteString)
import Data.List         (findIndex)

newtype Tag =
    Tag Int

-- TODO totality
getTag :: (Eq s, Monad m)
       => Type s -> s -> EitherT ByteString
                             (ReaderT [DataDefn s] m)
                                 Tag
getTag (TyCon typ _) name = do
    dataDefns <- lift ask
    let [DataDefn _ _ cs] = filter (\(DataDefn n _ _) -> n == typ) dataDefns
        Just i = findIndex (\(DataCon n _) -> n == name) cs
    pure $ Tag i
