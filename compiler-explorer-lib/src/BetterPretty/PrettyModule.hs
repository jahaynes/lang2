{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.PrettyModule where

import BetterPretty.Pretty
import BetterPretty.PrettyFunDefn
import Core.Definition

import Data.Text (Text)

newtype PrettyModule =
    PrettyModule (Module Text)

-- TODO data types & signatures
instance Pretty PrettyModule where
    pretty (PrettyModule modul) =
        concatMap pretty (PrettyFunDefn <$> getFunDefns modul)
