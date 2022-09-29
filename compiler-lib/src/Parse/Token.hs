module Parse.Token where

import Data.ByteString (ByteString)

data Token = TLet
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

           | TOr
           | TAnd

           | TEq
           | TColon
           | TArr

           | TPlusPlus
           | TPlus
           | TMul
           | TDiv

           | TDollar

           | TMinus
           | TNegate
           | TAmbiguous

           | TCase
           | TOf
               deriving (Eq, Read, Show)