{-# LANGUAGE OverloadedStrings #-}

module Pretty.Type where

import Core.Definition
import TypeCheck.Types

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

printTypeDefn :: (ByteString, Scheme) -> Builder
printTypeDefn (name, Forall vs typ) =
    let name'   = TB.text $ decodeUtf8 name
        vs'     = TB.intercalate " " $ map (TB.text . decodeUtf8) vs
        scheme' = case vs of
                      [] -> vs'
                      _  -> mconcat ["forall ", vs', ". "]
        typ'    = printType (decodeUtf8 <$> typ)
    in mconcat [name', " : ", scheme', typ']

printType :: Type Text -> Builder
printType (TyVar s) = TB.text s
printType (TyCon s) = TB.text s
printType (TyArr a b) = mconcat ["(", printType a, " -> ", printType b, ")"]

printTypeSig :: TypeSig Text -> Builder
printTypeSig (TypeSig name typ) =
    mconcat [TB.text name, " : ", printType typ]

printScheme :: Scheme -> Builder
printScheme (Forall [] t) = printType (fmap decodeUtf8 t)
printScheme (Forall vs t) = do
    let vs' = map (TB.text . decodeUtf8) vs
    "forall " <> TB.intercalate " " vs' <> ". " <> printType (fmap decodeUtf8 t)