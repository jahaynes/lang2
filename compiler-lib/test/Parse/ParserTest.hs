{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables #-}

module Parse.ParserTest (parserTests) where

import           Data.ByteString             (ByteString)
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
import Parse.Token

parserTests :: Group
parserTests =
    Group "Parsing" [ ("Simple expressions are parsed", prop_simpleExpressionsAreParsed)
                    , ("Division is left associative", prop_divisionLeftAssociative)
                    ]

prop_simpleExpressionsAreParsed :: Property
prop_simpleExpressionsAreParsed = property $ do
    num1 :: Integer <- forAll $ G.integral (linear 0 100)
    num2 :: Integer <- forAll $ G.integral (linear 0 100)

    let tokens = asTokens (printf "%d + %d" num1 num2)
    case runParser parseExpr tokens of
        Right (tokRemainder, (pos,EBinPrimOp AddI (ETerm (LitInt a)) (ETerm (LitInt b)))) -> do
            pos === SourcePos 1 1
            tokRemainder === []
            a === num1
            b === num2
        x -> error $ show x

prop_divisionLeftAssociative :: Property
prop_divisionLeftAssociative = property $ do
    let tokens = asTokens "8 / 4 / 2"
        Right ([], (SourcePos 1 1, expr)) = runParser parseExpr tokens
    expr ===
         EBinPrimOp DivI (EBinPrimOp DivI (ETerm (LitInt 8))
                                          (ETerm (LitInt 4)))
                         (ETerm (LitInt 2))

-- TODO
_prop_negativeNumbersAreParsed :: Property
_prop_negativeNumbersAreParsed = error "TODO"

asTokens :: String -> [(SourcePos, Token ByteString)]
asTokens input =
    case runLexer $ C8.pack input of
        Left e            -> error $ "Failed to tokenize: " <> e
        Right (_, tokens) -> tokens