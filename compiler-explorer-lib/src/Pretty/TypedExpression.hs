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

printTypedExpr :: TypedExpr Scheme Text -> (Atomic, Builder)
printTypedExpr (TermT _ term) =
    (Atom, printTerm term)

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