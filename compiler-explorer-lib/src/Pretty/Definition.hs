{-# LANGUAGE OverloadedStrings #-}

module Pretty.Definition where

import Core.Definition
import Core.Expression
import Pretty.Expression

import           Data.Text          (Text)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

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
