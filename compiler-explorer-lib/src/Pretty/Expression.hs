{-# LANGUAGE OverloadedStrings #-}

module Pretty.Expression where

import Core.Expression
import Core.Types (Untyped (..))
import Pretty.Common
import Pretty.Operator
import Pretty.Term

import           Data.Text    (Text)
import           Text.Builder (Builder)
import qualified Text.Builder as TB

-- TODO - is this dupe?
printExpr :: Show t => Expr t Text -> (Grouping, Builder)
printExpr (App _ f xs) =
    let f'  = group $ printExpr f
        xs' = TB.intercalate " " $ map (group . printExpr) xs
    in (Paren, f' <> " " <> xs')

printExpr (Lam _ vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printExpr body
    in (Paren, TB.intercalate " " ["\\" <> vs', "->", body'])

printExpr (UnPrimOp _ o e) =
    (Paren, TB.intercalate " " [printUnOp o, group $ printExpr e])

printExpr (BinPrimOp _ o a b) =
    let a' = group $ printExpr a
        b' = group $ printExpr b
    in (Paren, TB.intercalate " " [a', printBinOp o, b'])

printExpr (Let _ a b c) =
    let a' = TB.text a
        b' = group $ printExpr b
        c' = group $ printExpr c
    in (Paren, TB.intercalate " " ["let", a', "=", b', "in", c'])

printExpr (IfThenElse _ p t f) =
    let p' = group $ printExpr p
        t' = group $ printExpr t
        f' = group $ printExpr f
    in (Paren, TB.intercalate " " ["if", p', "then", t', "else", f'])

printExpr (Term _ t) =
    (Atom, printTerm t)

-- TODO
printExpr (Case _ scrut ps) =
    (Paren, TB.string (show ("case", scrut, ps)))