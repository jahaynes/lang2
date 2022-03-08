{-# LANGUAGE OverloadedStrings #-}

module Parse.ByteStringTest (byteStringTests) where

import qualified Parse.ByteString as B
import           Parse.Parser

import qualified Data.ByteString.Char8 as C8
import           Hedgehog
import qualified Hedgehog.Gen          as G
import           Hedgehog.Range              (linear)
import           Text.Printf                 (printf)

byteStringTests :: Group
byteStringTests =
    Group "Parser" [ ("dropWhile True drops everything", prop_dropWhileTrueDropsEverything)
                   , ("dropWhile False drops nothing", prop_dropWhileFalseDropsNothing)
                   , ("takeWhile True takes everything", prop_takeWhileTrueTakesEverything)
                   , ("takeWhile False takes nothing", prop_takeWhileFalseTakesNothing)
                   , ("string can prefix match", prop_stringCanPrefixMatch)
                   , ("string rejects mismatch", prop_stringRejectsMismatch)
                   , ("string handles running out", prop_stringHandlesRunningOut)
                   ]

prop_dropWhileTrueDropsEverything :: Property
prop_dropWhileTrueDropsEverything = property $ do
    chars <- forAll $ G.bytes (linear 0 20)
    let Right (Pos (Byte b) rm, ()) = runParser' (B.dropWhile (const True)) chars
    b            === C8.length chars
    C8.length rm === 0

prop_dropWhileFalseDropsNothing :: Property
prop_dropWhileFalseDropsNothing = property $ do
    chars <- forAll $ G.bytes (linear 0 20)
    let Right (Pos (Byte b) rm, ()) = runParser' (B.dropWhile (const False)) chars
    b  === 0
    rm === chars

prop_takeWhileTrueTakesEverything :: Property
prop_takeWhileTrueTakesEverything = property $ do
    chars <- forAll $ G.bytes (linear 0 20)
    let Right (Pos (Byte b1) rm, Pos (Byte b2) taken) = runParser' (B.takeWhile (const True)) chars
    b1           === C8.length chars
    b2           === 0
    C8.length rm === 0
    taken        === chars

prop_takeWhileFalseTakesNothing :: Property
prop_takeWhileFalseTakesNothing = property $ do
    chars <- forAll $ G.bytes (linear 0 20)
    let Right (Pos (Byte b1) rm, Pos (Byte b2) taken) = runParser' (B.takeWhile (const False)) chars
    b1              === 0
    b2              === 0
    C8.length rm    === C8.length chars
    C8.length taken === 0

prop_stringCanPrefixMatch :: Property
prop_stringCanPrefixMatch = property $ do
    target    <- forAll $ G.bytes (linear 0 20)
    remainder <- forAll $ G.bytes (linear 0 20)
    let Right (Pos (Byte b1) r, Pos (Byte b2) t) = runParser' (B.string target) (target <> remainder)
    b1 === C8.length target
    r  === remainder
    b2 === 0
    t  === target

prop_stringRejectsMismatch :: Property
prop_stringRejectsMismatch = property $ do
    needle   <- forAll . G.bytes $ linear 1 20
    haystack <- forAll . G.filter (\h -> not (needle `C8.isPrefixOf` h)) . G.bytes $ linear (C8.length needle) 20
    let Left x = runParser' (B.string needle) haystack
    C8.unpack x === printf "expected: %s at byte 0\n" (C8.unpack needle)

prop_stringHandlesRunningOut :: Property
prop_stringHandlesRunningOut = property $ do
    start <- forAll . G.bytes $ linear 1 20
    rest  <- forAll . G.bytes $ linear 1 20
    let needle = start <> rest
        Left x = runParser' (B.string needle) start
    C8.unpack x === printf "out of input for: %s at byte 0\n" (C8.unpack needle)

