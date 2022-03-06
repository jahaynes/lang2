module Parse2.Token2 where

import Data.ByteString (ByteString)

data Token = TBreak

           | TLet
           | TIn

           | TIf
           | TThen
           | TElse

           | TLambda
           | TDot

           | TLParen
           | TRParen

           | TLitBool Bool
           | TLitInt Integer
           | TLitString ByteString

           | TLowerStart ByteString
           | TUpperStart ByteString

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