{-# LANGUAGE OverloadedStrings #-}

module Pretty.Printer where

import Common.State
import Core.Definition
import Core.Expression
import Core.Term
import Core.Operator

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Text.Builder       (Builder)
import qualified Text.Builder as TB

--------------------------------

render :: [Defn ByteString] -> Text
render = TB.run . TB.intercalate "\n\n" . map go
    where
    go defn = fst $ runState (printDefn . fmap decodeUtf8 $ defn) 0

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

printExpr x = error $ show x

printTerm :: Term Text -> Builder
printTerm (LitBool True)  = "True"
printTerm (LitBool False) = "False"
printTerm (LitInt i)      = TB.decimal i
printTerm (LitString s)   = mconcat [TB.char '"', TB.text s, TB.char '"']
printTerm (Var v)         = TB.text v

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
        EqI     -> TB.string "=="
        LtEqI   -> TB.string "=<"
        LtI     -> TB.char   '<'
        GtEqI   -> TB.string ">="
        GtI     -> TB.char   '>'
        ConcatS -> TB.string "++"
