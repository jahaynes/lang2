{-# LANGUAGE OverloadedStrings #-}

module Pretty.Expression where

import Core.Expression
import Pretty.Common
import Pretty.Operator
import Pretty.Term

import           Data.Text    (Text)
import           Text.Builder (Builder)
import qualified Text.Builder as TB

printExpr :: Expr Text -> (Grouping, Builder)
printExpr (EApp f xs) =
    let f'  = group $ printExpr f
        xs' = TB.intercalate " " $ map (group . printExpr) xs
    in (Paren, f' <> " " <> xs')

printExpr (ELam vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printExpr body
    in (Paren, TB.intercalate " " ["\\" <> vs', "->", body'])

printExpr (EUnPrimOp o e) =
    (Paren, TB.intercalate " " [printUnOp o, group $ printExpr e])

printExpr (EBinPrimOp o a b) =
    let a' = group $ printExpr a
        b' = group $ printExpr b
    in (Paren, TB.intercalate " " [a', printBinOp o, b'])

printExpr (ELet a b c) =
    let a' = TB.text a
        b' = group $ printExpr b
        c' = group $ printExpr c
    in (Paren, TB.intercalate " " ["let", a', "=", b', "in", c'])

printExpr (IfThenElse p t f) =
    let p' = group $ printExpr p
        t' = group $ printExpr t
        f' = group $ printExpr f
    in (Paren, TB.intercalate " " ["if", p', "then", t', "else", f'])

printExpr (ETerm t) =
    (Atom, printTerm t)

printExpr (EClo fs vs body) = do
    let fs' = TB.intercalate " " $ map TB.text fs
        vs' = TB.intercalate " " $ map TB.text vs
        (_, body') = printExpr body
    (Paren, TB.intercalate " " ["\\[" <> fs' <> "] " <> vs', "->", body'])

printExpr (CallClo f fvs) =
    (Braces, TB.intercalate " " $ map TB.text (f:fvs))