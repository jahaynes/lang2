{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Parse.LexerTest where

import           Parse.Lexer
import qualified Parse.Parser as P
import           Parse.Token

import qualified Data.ByteString.Char8 as C8
import           Hedgehog
import qualified Hedgehog.Gen          as G

lexerTests :: Group
lexerTests =
    Group "Lexer" [ ("can lex tokens", prop_canLexTokens)
                  ]

prop_canLexTokens :: Property
prop_canLexTokens = property $ do

    (str, tok) <- forAll $ G.element [ ("let", TLet)
                                     , ("in",  TIn)
                                     , ("==",  TEqEq)
                                     , ("=",   TEq)
                                     , ("33",  TLitInt 33)
                                     , ("True", TLitBool True)
                                     , ("False", TLitBool False)
                                     ] -- TODO flesh out

    let Right (P.Pos (P.Byte b1) leftover, P.Pos (P.Byte b2) t) = P.runParser' token str
    leftover === ""
    b1       === C8.length str
    b2       === 0
    t        === tok
