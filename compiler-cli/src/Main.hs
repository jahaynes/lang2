{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse2.Expression
import Parse2.Lexer2
import Parse2.Parse2

main :: IO ()
main = do
    
    let tokens = runLexer "2 == 2 + 2"
    mapM_ print tokens
    
    case runParser parseExpr tokens of
        Left e -> error $ show e
        Right (_, Pos _ e) -> do
            print e
