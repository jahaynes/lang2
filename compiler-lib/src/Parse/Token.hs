module Parse.Token where

data Token s = LParen
             | RParen
             | TLet
             | In
             | TokInt Integer
             | TokBool Bool
             | TokString s
             | Lambda
             | Dot

             | DoubleEq
             | SingleEq

             | TLessEq
             | TLessThan
             | TGreaterEq
             | TGreaterThan

             | Plus
             | Minus
             | Times
             | Div
             | Mod

             | If
             | Then
             | Else
             | LowerIdent s
             | UpperIdent s
             | Comment s
             | TErr
             | TStitch
             | TShow
             | TData
             | TPipe
                 deriving (Eq, Show)
