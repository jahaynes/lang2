{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.Pretty where

import Data.Text as T (Text, replicate)
import Text.Builder   (Builder, run, text)

class Pretty a where

    pretty :: a -> [Segment]

data Segment = Code Builder
             | LParen
             | RParen
             | Space
             | PushDepth
             | Newline
             | IncIndent
             | PopDepth
                 deriving Show

render :: Pretty a => a -> Text
render = run . go [0] mempty . pretty
    where
    go               []   _      _ = error "No depth info"
    go                _ acc     [] = mconcat (reverse acc)
    go d@(depth:depth') acc (x:xs) =
        case x of

            Code c ->
                go d (c:acc) xs

            LParen ->
                go d ("(":acc) xs

            RParen ->
                go d (")":acc) xs

            Space ->
                go d (" ":acc) xs

            PushDepth ->
                go (depth:d) acc xs

            Newline ->
                let indent = text (T.replicate depth " ")
                    nl     = text "\n"
                in go d (indent:nl:acc) xs

            IncIndent ->
                go (depth+2:depth') acc xs

            PopDepth ->
                go depth' acc xs