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
test_string_match = unitTest $
    runLexer' (string "foo") "foo" ===
        Right ()

test_string_mismatch :: Property
test_string_mismatch = unitTest $
    Left "String mismatch" ===
        runLexer' (string "foo") "bar"

test_string_out_of_input :: Property
test_string_out_of_input = unitTest $
    runLexer' (string "foo") "fo" ===
        Left "Insufficient input"

test_integer :: Property
test_integer = unitTest $ do
    runLexer' integer    "" === Left "Expected digits"
    runLexer' integer "abc" === Left "Expected digits"
    runLexer' integer "123" === Right 123
    --runLexer' integer "123abc"  === Right 123 TODO should fail

test_booleans :: Property
test_booleans = unitTest $ do
    runLexer' boolean     ""  === Left "no alternatives left"
    runLexer' boolean  "True" === Right True
    runLexer' boolean "False" === Right False
    runLexer' boolean "Truef" === Left "no alternatives left"

test_get_position :: Property
test_get_position = unitTest $
    runLexer' lexer' "foobarbaz" ===
        Right (0, 6, 9)
    where
    lexer' = do
        p1 <- getPosition
        _  <- string "foobar"
        p2 <- getPosition
        _  <- string "baz"
        p3 <- getPosition
        pure (p1, p2, p3)

test_dropwhile :: Property
test_dropwhile = unitTest $
    runLexer' lexer' "aaabbb" ===
        Right (0, 0, 3, 6, 6)
    where
    lexer' = do
        p1 <- pDropWhile (const False) *> getPosition
        p2 <- pDropWhile (=='c')       *> getPosition
        p3 <- pDropWhile (=='a')       *> getPosition
        p4 <- pDropWhile (=='b')       *> getPosition
        p5 <- pDropWhile (const True)  *> getPosition
        pure (p1, p2, p3, p4, p5)

test_literal_string :: Property
test_literal_string = unitTest $ do

    runLexer' litString "" ===
        Left "Out of litString"

    runLexer' litString "abc" ===
        Left "Doesn't start with \""

    runLexer' litString "\"abc" ===
        Left "Ran off the end"

    runLexer' litString "\"foo bar\"" ===
        Right "foo bar"

    runLexer' litString "\"a \\\"quoted\\\" string\"" ===
        Right "a \\\"quoted\\\" string"

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p
