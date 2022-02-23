{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables #-}

module Parse.ParserTest (parserTests) where

import qualified Data.ByteString.Char8 as C8
import           Hedgehog
import qualified Hedgehog.Gen as G
import           Hedgehog.Range (linear)
import           Text.Printf    (printf)

import Core.Expression
import Core.Operator
import Core.Term
import Parse.ExpressionParser
import Parse.Lexer
import Parse.LexState
import Parse.Parser

parserTests :: Group
parserTests =
    Group "Parsing" [ ("Simple expressions are parsed", prop_simpleExpressionsAreParsed)
                    ]

prop_simpleExpressionsAreParsed :: Property
prop_simpleExpressionsAreParsed = property $ do
    num1 :: Integer <- forAll $ G.integral (linear 0 100)
    num2 :: Integer <- forAll $ G.integral (linear 0 100)
    let input = C8.pack $ printf "%d + %d" num1 num2
        Right (LexState _ strRemainder, tokens) = runLexer input
    strRemainder === ""
    case runParser parseExpr tokens of
        Right (tokRemainder, (pos,EBinPrimOp AddI (EApp (ETerm (LitInt a)) []) (EApp (ETerm (LitInt b)) []))) -> do
            pos === SourcePos 1 1
            tokRemainder === []
            a === num1
            b === num2
        x -> error $ show x

-- TODO
_prop_negativeNumbersAreParsed :: Property
_prop_negativeNumbersAreParsed = error "TODO"