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
printExpr :: Expr Untyped Text -> (Grouping, Builder)
printExpr (App Untyped f xs) =
    let f'  = group $ printExpr f
        xs' = TB.intercalate " " $ map (group . printExpr) xs
    in (Paren, f' <> " " <> xs')

printExpr (Lam Untyped vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printExpr body
    in (Paren, TB.intercalate " " ["\\" <> vs', "->", body'])

printExpr (UnPrimOp Untyped o e) =
    (Paren, TB.intercalate " " [printUnOp o, group $ printExpr e])

printExpr (BinPrimOp Untyped o a b) =
    let a' = group $ printExpr a
        b' = group $ printExpr b
    in (Paren, TB.intercalate " " [a', printBinOp o, b'])

printExpr (Let Untyped a b c) =
    let a' = TB.text a
        b' = group $ printExpr b
        c' = group $ printExpr c
    in (Paren, TB.intercalate " " ["let", a', "=", b', "in", c'])

printExpr (IfThenElse Untyped p t f) =
    let p' = group $ printExpr p
        t' = group $ printExpr t
        f' = group $ printExpr f
    in (Paren, TB.intercalate " " ["if", p', "then", t', "else", f'])

printExpr (Term Untyped t) =
    (Atom, printTerm t)

-- TODO
printExpr (Case Untyped scrut ps) =
    (Paren, TB.string (show ("case", scrut, ps)))