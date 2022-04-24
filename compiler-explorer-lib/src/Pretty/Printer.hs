{-# LANGUAGE OverloadedStrings #-}

module Pretty.Printer where

import Core.Definition
import Core.Expression
import Core.Term
import Core.Operator
import TypeCheck.TypedExpression
import TypeCheck.Types

import           Data.ByteString    (ByteString)
import qualified Data.Map    as M
import           Data.Text          (Text, pack)
import           Data.Text.Encoding (decodeUtf8)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

--------------------------------

render :: Module ByteString -> Text
render md =
    let txtModule = fmap decodeUtf8 md
        dataDefs  = map printDataDefn $ getDataDefns txtModule
        funDefs   = map printFunDefn  $ getFunDefns txtModule
    in TB.run . TB.intercalate "\n\n" $ concat [dataDefs, funDefs]

moduleToText :: Module ByteString -> Text
moduleToText md =
    let dataDefs = map (TB.text . pack . show) $ getDataDefns md
        funDefs  = map (TB.text . pack . show) $ getFunDefns md
    in TB.run . TB.intercalate "\n\n" $ concat [dataDefs, funDefs]

typedModuleToText :: TypedModule Scheme ByteString -> Text
typedModuleToText md =
    let dataDefs = map (TB.text . pack . show) $ getDataDefnsT md
        funDefs  = map (TB.text . pack . show) $ getFunDefnsT md
    in TB.run . TB.intercalate "\n\n" $ concat [dataDefs, funDefs]

renderTypeEnv :: TypeEnv -> Text
renderTypeEnv (TypeEnv env) = TB.run
                            . TB.intercalate "\n"
                            . map printTypeDefn
                            $ M.toList env

printTypeDefn :: (ByteString, Scheme) -> Builder
printTypeDefn (name, Forall vs typ) =
    let name'   = TB.text $ decodeUtf8 name
        vs'     = TB.intercalate " " $ map (TB.text . decodeUtf8) vs
        scheme' = case vs of
                      [] -> vs'
                      _  -> mconcat ["forall ", vs', ". "]
        typ'    = printType (decodeUtf8 <$> typ)
    in mconcat [name', " : ", scheme', typ']

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

printTypedExpr :: TypedExpr Scheme Text -> (Atomic, Builder)
printTypedExpr (TermT _ term) = (Atom, printTerm term)

printTypedExpr (LamT _ vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printTypedExpr body
    in (Group, TB.intercalate " " ["\\" <> vs', "->", body'])

printTypedExpr (AppT _ f xs) =
    let f'  = group $ printTypedExpr f
        xs' = TB.intercalate " " $ map (group . printTypedExpr) xs
    in (Group, f' <> " " <> xs')

printTypedExpr (LetT _ a b c) =
    let a' = TB.text a
        b' = group $ printTypedExpr b
        c' = group $ printTypedExpr c
    in (Group, TB.intercalate " " ["let", a', "=", b', "in", c'])

printTypedExpr (UnPrimOpT _ o e) =
    (Group, TB.intercalate " " [printUnOp o, group $ printTypedExpr e])

printTypedExpr (BinPrimOpT _ o a b) =
    let a' = group $ printTypedExpr a
        b' = group $ printTypedExpr b
    in (Group, TB.intercalate " " [a', printBinOp o, b'])

printTypedExpr (IfThenElseT _ p t f) =
    let p' = group $ printTypedExpr p
        t' = group $ printTypedExpr t
        f' = group $ printTypedExpr f
    in (Group, TB.intercalate " " ["if", p', "then", t', "else", f'])

--------------------------------

data Atomic = Atom | Group

group :: (Atomic, Builder) -> Builder
group (Atom, b)  = b
group (Group, b) = mconcat [TB.char '(', b, TB.char ')']

--------------------------------

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

printTypeSig :: TypeSig Text -> Builder
printTypeSig (TypeSig name typ) =
    mconcat [TB.text name, " : ", printType typ]

printScheme :: Scheme -> Builder
printScheme (Forall [] t) = printType (fmap decodeUtf8 t)
printScheme (Forall vs t) = do
    let vs' = map (TB.text . decodeUtf8) vs
    "forall " <> TB.intercalate " " vs' <> ". " <> printType (fmap decodeUtf8 t)

printType :: Type Text -> Builder
printType (TyVar s) = TB.text s
printType (TyCon s) = TB.text s
printType (TyArr a b) = mconcat ["(", printType a, " -> ", printType b, ")"]

printDataCon :: DataCon Text -> Builder
printDataCon (DataCon s members) =
    TB.intercalate " " (TB.text s : map printMember members)

printMember :: Member Text -> Builder
printMember m =
    case m of
        MemberType s -> TB.text s
        MemberVar s  -> TB.text s

printExpr :: Expr Text -> (Atomic, Builder)
printExpr (EApp f xs) =
    let f'  = group $ printExpr f
        xs' = TB.intercalate " " $ map (group . printExpr) xs
    in (Group, f' <> " " <> xs')

printExpr (ELam vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printExpr body
    in (Group, TB.intercalate " " ["\\" <> vs', "->", body'])

printExpr (EUnPrimOp o e) =
    (Group, TB.intercalate " " [printUnOp o, group $ printExpr e])

printExpr (EBinPrimOp o a b) =
    let a' = group $ printExpr a
        b' = group $ printExpr b
    in (Group, TB.intercalate " " [a', printBinOp o, b'])

printExpr (ELet a b c) =
    let a' = TB.text a
        b' = group $ printExpr b
        c' = group $ printExpr c
    in (Group, TB.intercalate " " ["let", a', "=", b', "in", c'])

printExpr (IfThenElse p t f) =
    let p' = group $ printExpr p
        t' = group $ printExpr t
        f' = group $ printExpr f
    in (Group, TB.intercalate " " ["if", p', "then", t', "else", f'])

printExpr (ETerm t) =
    (Atom, printTerm t)

printTerm :: Term Text -> Builder
printTerm t =
    case t of
        Var v         -> TB.text v
        DCons s       -> TB.text s
        LitInt i      -> TB.decimal i
        LitBool True  -> "True"
        LitBool False -> "False"
        LitString s   -> mconcat [TB.char '"', TB.text s, TB.char '"']

printUnOp :: UnOp -> Builder
printUnOp unOp =
    case unOp of
        Negate -> TB.char   '-'
        EShow  -> TB.string "show"
        Err    -> TB.string "error"

printBinOp :: BinOp -> Builder
printBinOp binOp =
    case binOp of
        AddI    -> TB.char   '+'
        SubI    -> TB.char   '-'
        MulI    -> TB.char   '*'
        DivI    -> TB.char   '/'
        ModI    -> TB.char   '%'
        EqA     -> TB.string "=="
        LtEqI   -> TB.string "=<"
        LtI     -> TB.char   '<'
        GtEqI   -> TB.string ">="
        GtI     -> TB.char   '>'
        AndB    -> TB.string "&&"
        OrB     -> TB.string "||"
        ConcatS -> TB.string "++"
