{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.PrettyTerm where

import BetterPretty.Pretty
import Core.Term

import Data.Text (Text)
import Text.Builder

newtype PrettyTerm =
    PrettyTerm (Term Text)

instance Pretty PrettyTerm where
    pretty (PrettyTerm term) =
        [Code $
            case term of
                Var v       -> text v
                DCons{}     -> error "TODO"
                LitInt n    -> decimal n
                LitBool b   -> if b then text "True" else text "False"
                LitString s -> char '"' <> text s <> char '"']