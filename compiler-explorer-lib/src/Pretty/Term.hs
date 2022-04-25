{-# LANGUAGE OverloadedStrings #-}

module Pretty.Term where

import Core.Term (Term (..))

import Data.Text    (Text)
import Text.Builder (Builder)
import Text.Builder (char, decimal, text)

printTerm :: Term Text -> Builder
printTerm t =
    case t of
        Var v         -> text v
        DCons s       -> text s
        LitInt i      -> decimal i
        LitBool True  -> "True"
        LitBool False -> "False"
        LitString s   -> mconcat [char '"', text s, char '"']
