{-# LANGUAGE OverloadedStrings #-}

module Pretty.TypedExpression where

import Pretty.Common
import Pretty.Operator
import Pretty.Term
import TypeCheck.TypedExpression
import TypeCheck.Types

import           Data.Text    (Text)
import           Text.Builder (Builder)
import qualified Text.Builder as TB

printTypedExpr :: TypedExpr Scheme Text -> (Grouping, Builder)
printTypedExpr (TermT _ term) =
    (Atom, printTerm term)

printTypedExpr (LamT _ vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printTypedExpr body
    in (Paren, TB.intercalate " " ["\\" <> vs', "->", body'])

printTypedExpr (AppT _ f xs) =
    let f'  = group $ printTypedExpr f
        xs' = TB.intercalate " " $ map (group . printTypedExpr) xs
    in (Paren, f' <> " " <> xs')

printTypedExpr (LetT _ a b c) =
    let a' = TB.text a
        b' = group $ printTypedExpr b
        c' = group $ printTypedExpr c
    in (Paren, TB.intercalate " " ["let", a', "=", b', "in", c'])

printTypedExpr (UnPrimOpT _ o e) =
    (Paren, TB.intercalate " " [printUnOp o, group $ printTypedExpr e])

printTypedExpr (BinPrimOpT _ o a b) =
    let a' = group $ printTypedExpr a
        b' = group $ printTypedExpr b
    in (Paren, TB.intercalate " " [a', printBinOp o, b'])

printTypedExpr (IfThenElseT _ p t f) =
    let p' = group $ printTypedExpr p
        t' = group $ printTypedExpr t
        f' = group $ printTypedExpr f
    in (Paren, TB.intercalate " " ["if", p', "then", t', "else", f'])