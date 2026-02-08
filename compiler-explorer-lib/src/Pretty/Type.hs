{-# LANGUAGE OverloadedStrings #-}

module Pretty.Type where

import Core.Types
import Pretty.Common

import           Data.ByteString   (ByteString)
import           TextBuilder       (TextBuilder)
import qualified TextBuilder as TB

printPolyType :: Polytype ByteString -> TextBuilder
printPolyType (Forall [] t) = printType t
printPolyType (Forall q  t) = mconcat ["forall ", printVars q, ". ", printType t]

printType :: Type ByteString -> TextBuilder
printType t =
    case t of
        TyArr a b -> mconcat [go a, " -> ", go b]
        _         -> go t

    where
    go (TyArr a b)   = mconcat ["(", go a, " -> ", go b, ")"]
    go (TyCon c tvs) = TB.intercalate " " (bytestring c: map go tvs)
    go (TyVar v)     = bytestring v
