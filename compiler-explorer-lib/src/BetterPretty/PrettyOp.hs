{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.PrettyOp where

import BetterPretty.Pretty
import Core.Operator

newtype PrettyUnOp =
    PrettyUnOp UnOp

instance Pretty PrettyUnOp where
    pretty (PrettyUnOp op) =
        [ Code $ case op of
            Negate -> "-" ]

newtype PrettyBinOp =
    PrettyBinOp BinOp

instance Pretty PrettyBinOp where
    pretty (PrettyBinOp op) =
        [ Code $ case op of
                     AddI -> "+"
                     SubI -> "-"
                     EqA  -> "==" ]
