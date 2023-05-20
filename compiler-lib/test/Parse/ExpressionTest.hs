{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Parse.ExpressionTest where

import           Core.Expression
import           Core.Operator
import           Core.Term
import           Parse.LexAndParse
import           Parse.Lexer
import           Parse.Expression
import           Parse.Parser
import           Parse.Token

import           Data.ByteString (ByteString)
import           Data.Vector     (Vector)
import           Hedgehog hiding (Var)

exprTests :: Group
exprTests =
    Group "Expr" [ ("variable_match", test_variable_match)
                 , ("variable_mismatch", test_variable_mismatch)
                 , ("neg_variable_match", test_neg_variable_match)
                 , ("lit_string", test_lit_string)
                 , ("lit_bool", test_lit_bool)
                 , ("lit_int", test_lit_int)
                 , ("fun_app", test_fun_app) ]

test_variable_match :: Property
test_variable_match = unitTest $
    let x = parse' undefined undefined [TLowerStart "abc"] parseVariable
    in x === Right (ETerm (Var "abc"))

test_variable_mismatch :: Property
test_variable_mismatch = unitTest $
    let x = parse' undefined undefined [TEqEq] parseVariable
    in x === Left "no alternatives left"

test_neg_variable_match :: Property
test_neg_variable_match = unitTest $
    let x = parse' undefined undefined [TNegate, TLowerStart "def"] parseVariable
    in x === Right (EUnPrimOp Negate (ETerm (Var "def")))

test_lit_string :: Property
test_lit_string = unitTest $
    let x = parse' undefined undefined [TLitString "xy"] parseLiteral
    in x === Right (ETerm (LitString "xy"))

test_lit_bool :: Property
test_lit_bool = unitTest $ do
    let x = parse' undefined undefined [TLitBool True] parseLiteral
        y = parse' undefined undefined [TLitBool False] parseLiteral
    x === Right (ETerm (LitBool True))
    y === Right (ETerm (LitBool False))

test_lit_int :: Property
test_lit_int = unitTest $ do
    let x = parse' undefined undefined [TLitInt 4] parseLiteral
        y = parse' undefined undefined [TNegate, TLitInt 5] parseLiteral
    x === Right (ETerm (LitInt 4))
    y === Right (ETerm (LitInt (-5)))

test_fun_app :: Property
test_fun_app = unitTest $ do
    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parseApply "f x y"
    tokens === fmap TLowerStart ["f", "x", "y"] 
    pr     === EApp (ETerm (Var "f")) [ETerm (Var "x"),ETerm (Var "y")]

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p

lexAndParseWith :: Parser ParseState a
                -> ByteString
                -> Either ByteString (Vector Token, Either ByteString (ParseState, a))
lexAndParseWith p source = do
    let lineStarts = findLineStarts source
    (positions, tokens) <- runLexer source
    let pr = runParser p $ ParseState { ps_tokens     = tokens
                                      , ps_pos        = 0
                                      , ps_positions  = positions
                                      , ps_lineStarts = lineStarts
                                      }
    pure (tokens, pr)
