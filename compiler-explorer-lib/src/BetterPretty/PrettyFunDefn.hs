{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.PrettyFunDefn where

import BetterPretty.Pretty
import BetterPretty.PrettyExpr
import Core.Definition
import Core.Expression

import Data.Text (Text)
import Text.Builder

newtype PrettyFunDefn =
    PrettyFunDefn (FunDefn Text)

instance Pretty PrettyFunDefn where

    pretty (PrettyFunDefn (FunDefn n (ELam vs e))) =
        let vs' = intercalate " " (map text vs)
            sig = intercalate " " [ text n, vs', "=" ]
            e'  = pretty (PrettyExpr e)
        in [ PushDepth, Code sig, IncIndent, Newline ] ++ e' ++ [PopDepth, Newline, Newline]

    pretty (PrettyFunDefn (FunDefn n e)) =
        let sig = intercalate " " [ text n, "=" ]
            e'  = pretty (PrettyExpr e)
        in [ PushDepth, Code sig, IncIndent, Newline ] ++ e' ++ [PopDepth, Newline, Newline]
