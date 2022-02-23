{-# LANGUAGE OverloadedStrings #-}

module Parse.LexerTest (lexerTests) where

import qualified Data.ByteString.Char8 as C8
import           Data.Set (Set)
import qualified Data.Set as S
import           Hedgehog
import qualified Hedgehog.Gen as G
import           Hedgehog.Range (linear)

import Parse.Lexer
import Parse.LexState
import Parse.Token

lexerTests :: Group
lexerTests =
    Group "Lexing" [ ("Source Position is accurate", prop_sourcePosIsAccurate)
                   , ("Tokens are lexed", prop_identifiersAreLexed)
                   ]

prop_identifiersAreLexed :: Property
prop_identifiersAreLexed = property $ do
    chars <- forAll . G.filter (`S.notMember` keywords)
                    . G.list (linear 1 10)
                    $ G.lower
    let input = C8.pack chars
    let Right (LexState _ remainder, [(_, output)]) = runLexer input
    remainder === ""
    output    === LowerIdent input

    where
    keywords :: Set String
    keywords = S.fromList ["let", "in", "if", "then", "else", "err", "show", "data"]

prop_sourcePosIsAccurate :: Property
prop_sourcePosIsAccurate = property $ do
    numLines  <- forAll $ G.integral (linear 0 100)
    numSpaces <- forAll $ G.integral (linear 0 100)
    let input = C8.replicate numLines '\n' <> C8.replicate numSpaces ' ' <> "3"
        tokenPosition  = SourcePos (numLines + 1) (numSpaces + 1)
        cursorPosition = SourcePos (numLines + 1) (numSpaces + 2)
    runLexer input ===
        Right (LexState cursorPosition "", [(tokenPosition, TokInt 3)])
