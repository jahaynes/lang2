module Parse2.Token2 where

import Data.ByteString (ByteString)

data Token = TBreak
           | TLet
           | TIn
           | TLitBool Bool
           | TLitInt Integer
           | TLitString ByteString

           | TEqEq
           | TGt
           | TGtEq
           | TLt
           | TLtEq

           | TEq
           | TPlus
           | TMinus
           | TMul
           | TDiv
               deriving (Eq, Read, Show)