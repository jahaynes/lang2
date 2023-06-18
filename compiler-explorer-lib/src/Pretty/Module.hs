{-# LANGUAGE OverloadedStrings #-}

module Pretty.Module where

import Core.Expression
import Core.Module
import Pretty.Expression

import           Data.ByteString    (ByteString)
import           Data.Text          (Text, pack)
import           Data.Text.Encoding (decodeUtf8)
import qualified Text.Builder as TB
import           Text.Builder       (Builder)

render :: Show t => Module t ByteString -> Text
render md =
    let txtModule = fmap decodeUtf8 md
        dataDefs  = map printDataDefn $ getDataDefns txtModule
        funDefs   = map printFunDefn  $ getFunDefns txtModule
    in TB.run . TB.intercalate "\n\n" $ mconcat [dataDefs, funDefs]

moduleToText :: Show t => Module t ByteString -> Text
moduleToText md =
    let dataDefs = map (TB.text . pack . show) $ getDataDefns md
        funDefs  = map (TB.text . pack . show) $ getFunDefns md
    in TB.run . TB.intercalate "\n\n" $ mconcat [dataDefs, funDefs]

printFunDefn :: Show t => FunDefn t Text -> Builder
printFunDefn (FunDefn f _ (Lam _ vs x)) = -- TODO quant
    let fvs'    = TB.intercalate " " $ map TB.text (f:vs)
        (_, x') = printExpr x
    in mconcat [fvs', " = ", x']

printFunDefn (FunDefn f _ x) = -- TODO quant
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
        MemberType tc tvs ->
            TB.intercalate " " (TB.text tc:map printMember tvs)
        MemberVar s ->
            TB.text s
