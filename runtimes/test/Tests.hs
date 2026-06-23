{-# LANGUAGE OverloadedStrings #-}

import Hedgehog (checkParallel)

import Data.IntSet (IntSet)

import Parse.LexAndParse (findLineStarts)
import Parse.Lexer (runLexer)
import Parse.Module (parseDefns)
import Parse.Parser (doParse)
import Phase.CodeGen.Direct (codeGen)
import Phase.EtaExpand.EtaExpand (etaExpand)
import Runtimes.DirectMachine (runMain, showValue)
import TypeSystem.TypeCheck (inferModule)

import Data.ByteString (ByteString)
import Data.Vector (Vector)

main :: IO ()
main = do
    testSummorial
    mapM_ checkParallel []

testSummorial :: IO ()
testSummorial = do
    let src = "summorial m =\n    let go acc n =\n        if n == 0\n            then acc\n            else go (acc + n) (n - 1) in\n    go 0 m\n\nmain = summorial 10" :: ByteString
    case runLexer src of
        Left err -> putStrLn $ "FAIL: Lexer error: " ++ show err
        Right (positions, tokens) ->
            let lineStarts = findLineStarts src
            in case doParse parseDefns positions tokens lineStarts of
                Left err -> putStrLn $ "FAIL: Parser error: " ++ show err
                Right modul ->
                    case inferModule modul of
                        Left err -> putStrLn $ "FAIL: Type error: " ++ show err
                        Right typedModule -> do
                            let etaExpanded = etaExpand typedModule
                            case codeGen etaExpanded of
                                Left err -> putStrLn $ "FAIL: CodeGen error: " ++ show err
                                Right code -> do
                                    putStrLn $ "Code: " ++ take 200 (show code)
                                    case runMain code of
                                        Left err -> putStrLn $ "FAIL: Runtime error: " ++ err
                                        Right val ->
                                            let result = showValue val
                                            in if result == "55"
                                               then putStrLn $ "PASS: summorial 10 = " ++ show result
                                               else putStrLn $ "FAIL: Expected 55, got " ++ show result
