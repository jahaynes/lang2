module Phase.CodeGen.Direct where

import Core.Module

import Data.ByteString (ByteString)

data Code = Code

codeGen :: Module t s -> Either ByteString Code
codeGen modu = Right Code