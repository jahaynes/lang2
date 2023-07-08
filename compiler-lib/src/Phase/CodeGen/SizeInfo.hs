module Phase.CodeGen.SizeInfo where

import Core.Module
import Core.Term
import Core.Types

import           Data.Map (Map)
import qualified Data.Map as M

-- How many bytes is needed to allocate the object
newtype AllocSz =
    AllocSz Int

-- How many bytes is needed to store the object
-- This will either be a pointer or a primitive,
-- so always 8 for now
newtype StoreSz =
    StoreSz Int

calculateDataConsSizes :: [DataDefn s] -> Map (s, Type s) AllocSz
calculateDataConsSizes _dataDefns = error "calculateDataConsSizes"

-- 'Default' because primitives will be unpacked
-- and objects will be behind pointers
-- unless some unpacking is implemented in future
getDefaultStoreSize :: Term s -> Type s -> StoreSz
getDefaultStoreSize _term _type = error "getDefaultStoreSize"

getAllocationSzBytes :: Term s -> Type s -> Maybe StoreSz
getAllocationSzBytes _term _type = error "getAllocationSzBytes"

getPointerSzBytes :: Int
getPointerSzBytes = 8