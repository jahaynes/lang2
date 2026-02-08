{-# LANGUAGE OverloadedStrings #-}

module Pretty.Term where

import Core.Term (Term (..))

import Data.Text   (Text)
import TextBuilder (TextBuilder, char, decimal, text)

printTerm :: Term Text -> TextBuilder
printTerm t =
    case t of
        Var v         -> text v
        DCons s       -> text s
        LitInt i      -> decimal i
        LitBool True  -> "True"
        LitBool False -> "False"
        LitString s   -> mconcat [char '"', text s, char '"']
