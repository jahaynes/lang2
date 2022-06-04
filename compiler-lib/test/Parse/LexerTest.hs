{-# LANGUAGE OverloadedStrings #-}

module Parse.LexerTest where

import           Parse.Lexer

import           Hedgehog

lexerTests :: Group
lexerTests =
    Group "Lexer3" [ ("string_match", test_string_match)
                   , ("string_mismatch", test_string_mismatch)
                   , ("string_out_of_input", test_string_out_of_input)
                   , ("integer", test_integer)
                   , ("booleans", test_booleans)
                   , ("get_position", test_get_position)
                   , ("dropwhile", test_dropwhile)
                   , ("literal_string", test_literal_string)
                   ]

test_string_match :: Property
test_string_match = unitTest $ do
    let x = lex' "foo" $ string "foo"
    x === Right ()

test_string_mismatch :: Property
test_string_mismatch = unitTest $ do
    let x = lex' "bar" $ string "foo"
    x === Left "String mismatch"

test_string_out_of_input :: Property
test_string_out_of_input = unitTest $ do
    let x = lex' "fo" $ string "foo"
    x === Left "Insufficient input"

test_integer :: Property
test_integer = unitTest $ do
    lex'    "" integer === Left "Expected digits"
    lex' "abc" integer === Left "Expected digits"
    lex' "123" integer === Right 123
    --lex' "123abc" integer === Right 123 TODO should fail

test_booleans :: Property
test_booleans = unitTest $ do
    lex'      "" boolean === Left "no alternatives left"
    lex'  "True" boolean === Right True
    lex' "False" boolean === Right False
    lex' "Truef" boolean === Left "no alternatives left"

test_get_position :: Property
test_get_position = unitTest $ do
    let x = lex' "foobarbaz" $ do
                p1 <- getPosition
                _  <- string "foobar"
                p2 <- getPosition
                _  <- string "baz"
                p3 <- getPosition
                pure (p1, p2, p3)
    x === Right (0, 6, 9)

test_dropwhile :: Property
test_dropwhile = unitTest $
    let Right r =
            lex' "aaabbb" $ do
                p1 <- pDropWhile (const False) *> getPosition
                p2 <- pDropWhile (=='c')       *> getPosition
                p3 <- pDropWhile (=='a')       *> getPosition
                p4 <- pDropWhile (=='b')       *> getPosition
                p5 <- pDropWhile (const True)  *> getPosition
                pure (p1, p2, p3, p4, p5)
    in r === (0, 0, 3, 6, 6)

test_literal_string :: Property
test_literal_string = unitTest $ do

    lex' "" litString ===
        Left "Out of litString"

    lex' "abc" litString ===
        Left "Doesn't start with \""

    lex' "\"abc" litString ===
        Left "Ran off the end"

    lex' "\"foo bar\"" litString ===
        Right "foo bar"

    lex' "\"a \\\"quoted\\\" string\"" litString ===
        Right "a \\\"quoted\\\" string"

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p
