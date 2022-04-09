{-# LANGUAGE OverloadedStrings #-}

module Pretty.Printer where

import Common.State
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
render = TB.run . TB.intercalate "\n\n" . map go
    where
    go defn = fst $ runState (printDefn . fmap decodeUtf8 $ defn) 0

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

printDefn :: Defn Text -> State Int Builder

printDefn (FunDefn f (ELam vs x)) = do
    let fvs' = TB.intercalate " " $ map TB.text (f:vs)
    (_, x') <- printExpr x
    pure $ mconcat [fvs', " = ", x']

printDefn (FunDefn f x) = do
    let f' = TB.text f
    (_, x') <- printExpr x
    pure $ mconcat [f', " = ", x']

printDefn (DataDefn t tyvars dataCons) = do
    let t'       = TB.text t
        tyvars'  = map TB.text tyvars
        ttyvars' = TB.intercalate " " (t':tyvars')
        dcs'     = TB.intercalate " | " $ map printDataCon dataCons
    pure $ mconcat [ttyvars', " = ", dcs']

printDefn (TypeSig name typ) = do
    let name'    = TB.text name
        typ'     = printType typ
    pure $ mconcat [name', " : ", typ']

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

printExpr :: Expr Text -> State Int (Atomic, Builder)
printExpr (EApp f xs) = do
    f'  <- group <$> printExpr f
    xs' <- TB.intercalate " " . map group <$> mapM printExpr xs
    pure $ (Group, f' <> " " <> xs')

printExpr (ELam vs body) = do
    let vs' = TB.intercalate " " $ map TB.text vs
    (_, body') <- printExpr body
    pure (Group, TB.intercalate " " ["\\" <> vs', "->", body'])

printExpr (EUnPrimOp o e) = do
    e' <- group <$> printExpr e
    pure (Group, TB.intercalate " " [printUnOp o, e'])

printExpr (EBinPrimOp o a b) = do
    a' <- group <$> printExpr a
    b' <- group <$> printExpr b
    pure (Group, TB.intercalate " " [a', printBinOp o, b'])

printExpr (ELet a b c) = do
    let a' = TB.text a
    b' <- group <$> printExpr b
    c' <- group <$> printExpr c
    pure (Group, TB.intercalate " " ["let", a', "=", b', "in", c'])

printExpr (IfThenElse p t f) = do
    p' <- group <$> printExpr p
    t' <- group <$> printExpr t
    f' <- group <$> printExpr f
    pure (Group, TB.intercalate " " ["if", p', "then", t', "else", f'])

printExpr (ETerm t) =
    pure (Atom, printTerm t)

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
