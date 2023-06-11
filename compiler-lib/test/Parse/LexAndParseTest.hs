{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Parse.LexAndParseTest where

import           Core.Expression
import           Core.Module
import           Core.Term
import           Core.Types (Untyped (..))
import           Parse.Expression
import           Parse.LexAndParse
import           Parse.Lexer
import           Parse.Module
import           Parse.Parser
import           Parse.Token

import           Data.ByteString (ByteString)
import           Data.Vector (Vector)
import           Hedgehog

lexAndParseTests :: Group
lexAndParseTests =
    Group "LexAndParse" [ ("simpleDefinition", prop_simple_definition)
                        , ("mr_more_right", test_mr_more_right)
                        , ("mr_same_column", test_mr_same_column)  
                        , ("mr_diag_right", test_mr_diag_right)
                        , ("mr_diag_left", test_mr_diag_left)
                        , ("mr_none", test_mr_none)
                        ]

prop_simple_definition :: Property
prop_simple_definition = unitTest $ do

    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parseFunDefn "x = 5"

    tokens === [TLowerStart "x", TEq, TLitInt 5]
    pr === FunDefn "x" (TermT Untyped (LitInt 5))

test_mr_more_right :: Property
test_mr_more_right = unitTest $ do

    let parser = parseWhileColumns MoreRight parseLowerStart

    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parser "left right"

    tokens === [TLowerStart "left", TLowerStart "right"]
    pr === ["left", "right"]

test_mr_same_column :: Property
test_mr_same_column = unitTest $ do

    let parser = do x <- parseWhileColumns MoreRight parseLowerStart
                    y <- parseWhileColumns MoreRight parseLowerStart
                    pure (x, y)

    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parser "up  \n\
                                   \down  "

    tokens === [TLowerStart "up", TLowerStart "down" ]
    pr === (["up"], ["down"])

test_mr_diag_right :: Property
test_mr_diag_right = unitTest $ do

    let parser = parseWhileColumns MoreRight parseLowerStart

    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parser "topleft \n\
                                   \ bottomright"

    tokens === [TLowerStart "topleft", TLowerStart "bottomright" ]
    pr === ["topleft", "bottomright"]

test_mr_diag_left :: Property
test_mr_diag_left = unitTest $ do

    let parser = parseWhileColumns MoreRight parseLowerStart

    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parser " topright \n\
                                   \bottomleft"

    tokens === [TLowerStart "topright", TLowerStart "bottomleft" ]
    pr === ["topright"]

test_mr_none :: Property
test_mr_none = unitTest $ do

    let parser = do upper <- parseUpperStart
                    _     <- parseWhileColumns MoreRight parseUpperStart
                    lower <- parseLowerStart
                    pure (upper, lower)

    let Right (tokens, Right (_, pr)) =
            lexAndParseWith parser "F y"

    tokens === [TUpperStart "F", TLowerStart "y"]

    pr === ("F", "y")

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

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property
