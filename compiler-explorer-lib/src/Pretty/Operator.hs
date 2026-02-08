{-# LANGUAGE OverloadedStrings #-}

module Pretty.Operator where

import Core.Operator

import           TextBuilder (TextBuilder)
import qualified TextBuilder as TB

printUnOp :: UnOp -> TextBuilder
printUnOp unOp =
    case unOp of
        Negate -> TB.char   '-'
        EShow  -> TB.string "show"
        Err    -> TB.string "error"

printBinOp :: BinOp -> TextBuilder
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
