{-# LANGUAGE OverloadedStrings #-}

module Pretty.Definition where

import Core.Definition
import Core.Expression
import Pretty.Common
import Pretty.Expression
import Pretty.Type
import Pretty.TypedExpression
import TypeCheck.TypedExpression
import TypeCheck.Types

import           Data.ByteString    (ByteString)
import qualified Data.Map    as M
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

renderTypeEnv :: TypeEnv -> Text
renderTypeEnv (TypeEnv env) = TB.run
                            . TB.intercalate "\n"
                            . map printTypeDefn
                            $ M.toList env

renderTypedDefns :: [FunDefnT Scheme ByteString] -> Text
renderTypedDefns = TB.run . TB.intercalate "\n\n" . map renderTypedDefn

renderTypedDefn :: FunDefnT Scheme ByteString -> Builder
renderTypedDefn (FunDefnT ty name ex) =
    let name'   = TB.text $ decodeUtf8 name
        typeSig = name' <> " : " <> printScheme ty
        ex'     = renderTypedExpr ex
    in TB.intercalate "\n" [typeSig, name' <> " = " <> ex']

renderTypedExpr :: TypedExpr Scheme ByteString -> Builder
renderTypedExpr = group . printTypedExpr . fmap decodeUtf8

printFunDefn :: FunDefn Text -> Builder
printFunDefn (FunDefn f (ELam vs x)) =
    let fvs'    = TB.intercalate " " $ map TB.text (f:vs)
        (_, x') = printExpr x
    in mconcat [fvs', " = ", x']

printFunDefn (FunDefn f x) =
    let f'      = TB.text f
        (_, x') = printExpr x
    in mconcat [f', " = ", x']

printDataDefn :: DataDefn Text -> Builder
printDataDefn (DataDefn t tyvars dataCons) =
    let t'       = TB.text t
        tyvars'  = map TB.text tyvars
        ttyvars' = TB.intercalate " " (t':tyvars')
        dcs'     = TB.intercalate " | " $ map printDataCon dataCons
    in mconcat [ttyvars', " = ", dcs']

printDataCon :: DataCon Text -> Builder
printDataCon (DataCon s members) =
    TB.intercalate " " (TB.text s : map printMember members)

printMember :: Member Text -> Builder
printMember m =
    case m of
        MemberType s -> TB.text s
        MemberVar s  -> TB.text s
