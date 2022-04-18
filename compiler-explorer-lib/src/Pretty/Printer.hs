{-# LANGUAGE OverloadedStrings #-}

module Pretty.Printer where

import Core.Definition
import Core.Expression
import Core.Term
import Core.Operator
import TypeCheck.Types

import           Data.ByteString    (ByteString)
import qualified Data.Map    as M
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

--------------------------------

render :: [Defn ByteString] -> Text
render = TB.run
       . TB.intercalate "\n\n"
       . map (printDefn . fmap decodeUtf8)

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

--------------------------------

data Atomic = Atom | Group

group :: (Atomic, Builder) -> Builder
group (Atom, b)  = b
group (Group, b) = mconcat [TB.char '(', b, TB.char ')']

--------------------------------

printDefn :: Defn Text -> Builder

printDefn (FunDefn f (ELam vs x)) =
    let fvs'    = TB.intercalate " " $ map TB.text (f:vs)
        (_, x') = printExpr x
    in mconcat [fvs', " = ", x']

printDefn (FunDefn f x) =
    let f'      = TB.text f
        (_, x') = printExpr x
    in mconcat [f', " = ", x']

printDefn (DataDefn t tyvars dataCons) =
    let t'       = TB.text t
        tyvars'  = map TB.text tyvars
        ttyvars' = TB.intercalate " " (t':tyvars')
        dcs'     = TB.intercalate " | " $ map printDataCon dataCons
    in mconcat [ttyvars', " = ", dcs']

printDefn (TypeSig name typ) =
    mconcat [TB.text name, " : ", printType typ]

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
