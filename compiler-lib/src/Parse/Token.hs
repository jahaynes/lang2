module Parse.Token where

import Data.ByteString (ByteString)

data Token = TBreak

           | TLet
           | TIn

           | TIf
           | TThen
           | TElse

           | TLambda
           | TDot
           | TPipe

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
           | TMul
           | TDiv

           | TMinus
           | TNegate
           | TAmbiguous
               deriving (Eq, Read, Show)