{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.PrettyExpr where

import BetterPretty.Pretty
import BetterPretty.PrettyOp
import BetterPretty.PrettyTerm
import Core.Expression

import           Data.Text      (Text)
import qualified Data.List as L
import           Text.Builder

newtype PrettyExpr =
    PrettyExpr (Expr Text)

instance Pretty PrettyExpr where

    pretty (PrettyExpr expr) =

        case expr of

            ETerm t ->
                pretty (PrettyTerm t)

            ELam vs e ->
                let vs' = Code $ intercalate " " (map text vs)
                    e'  = pretty (PrettyExpr e)
                in vs':e'

            EApp f xs ->
                let f'  =      pretty (PrettyExpr f)
                    xs' = map (pretty . PrettyExpr) xs
                in L.intercalate [Space] (f':xs')

            -- Special case let-lambda to look like named function definition
            ELet a (ELam vs b) c ->
                let vs' = intercalate " " (map text vs)
                    sig = intercalate " " ["let", text a, vs', "="]
                    b'  = pretty (PrettyExpr b)
                    c'  = pretty (PrettyExpr c)
                in  [ PushDepth, Code sig, IncIndent, Newline ]
                 ++ b'
                 ++ [ Code " in", PopDepth, Newline ]
                 ++ c'

            -- Regular let
            ELet a b c ->
                let sig = intercalate " " ["let", text a, "="]
                    b'  = pretty (PrettyExpr b)
                    c'  = pretty (PrettyExpr c)
                in  [ Code sig, Space ]
                 ++ b'
                 ++ [ Space, Code "in", Newline ]
                 ++ c'

            EUnPrimOp op e ->
                let op' = pretty (PrettyUnOp op)
                    e'  = pretty (PrettyExpr e)
                in concat [[LParen], op', [Space], e', [RParen]]

            EBinPrimOp op a b ->
                let a'  = pretty (PrettyExpr a)
                    op' = pretty (PrettyBinOp op)
                    b'  = pretty (PrettyExpr b)
                in concat [[LParen], a', [Space], op', [Space], b', [RParen]]

            IfThenElse pr tr fl ->
                let pr' = pretty (PrettyExpr pr)
                    tr' = pretty (PrettyExpr tr)
                    fl' = pretty (PrettyExpr fl)
                in concat [ [PushDepth]
                          , [Code "if", Space], pr', [IncIndent]
                          , [Newline, Code "then", Space], tr'
                          , [Newline, Code "else", Space], fl'
                          , [PopDepth] ]

            EClo{} ->
                error "closure"

            CallClo{} ->
                error "closure"
