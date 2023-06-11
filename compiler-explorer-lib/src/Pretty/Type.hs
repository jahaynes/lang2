{-# LANGUAGE OverloadedStrings #-}

module Pretty.Type where

import Core.Types
import Pretty.Common

import           Data.ByteString       (ByteString)
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

printPolyType :: Polytype ByteString -> Builder
printPolyType (Forall [] t) = printType t
printPolyType (Forall q  t) = mconcat ["forall ", printVars q, ". ", printType t]

printType :: Type ByteString -> Builder
printType = TB.intercalate " -> " . unbuild []
    where
    unbuild acc (TyArr a b) = unbuild (a:acc) b
    unbuild acc           t = reverse $ map prt (t:acc)

    prt (TyCon c tvs) = TB.intercalate " " (bytestring c: map prt tvs)
    prt (TyVar v)     = bytestring v
    prt t@TyArr{}     = mconcat ["(", printType t,")"]
