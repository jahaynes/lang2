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

-- TODO - is this dupe of Pretty (ExprT) ?
printExpr :: ExprT Untyped Text -> (Grouping, Builder)
printExpr (AppT Untyped f xs) =
    let f'  = group $ printExpr f
        xs' = TB.intercalate " " $ map (group . printExpr) xs
    in (Paren, f' <> " " <> xs')

printExpr (LamT Untyped vs body) =
    let vs'        = TB.intercalate " " $ map TB.text vs
        (_, body') = printExpr body
    in (Paren, TB.intercalate " " ["\\" <> vs', "->", body'])

printExpr (UnPrimOpT Untyped o e) =
    (Paren, TB.intercalate " " [printUnOp o, group $ printExpr e])

printExpr (BinPrimOpT Untyped o a b) =
    let a' = group $ printExpr a
        b' = group $ printExpr b
    in (Paren, TB.intercalate " " [a', printBinOp o, b'])

printExpr (LetT Untyped a b c) =
    let a' = TB.text a
        b' = group $ printExpr b
        c' = group $ printExpr c
    in (Paren, TB.intercalate " " ["let", a', "=", b', "in", c'])

printExpr (IfThenElseT Untyped p t f) =
    let p' = group $ printExpr p
        t' = group $ printExpr t
        f' = group $ printExpr f
    in (Paren, TB.intercalate " " ["if", p', "then", t', "else", f'])

printExpr (TermT Untyped t) =
    (Atom, printTerm t)

-- TODO
printExpr (CaseT Untyped scrut ps) =
    (Paren, TB.string (show ("case", scrut, ps)))