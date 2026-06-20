{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables #-}

module Runtimes.MachineATest (machineATests) where

import Runtimes.MachineA

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

machineATests :: Group
machineATests =
    Group "machineA" [ ("test_int_bytes_roundtrip", test_int_bytes_roundtrip) ]

test_int_bytes_roundtrip :: Property
test_int_bytes_roundtrip = withTests 20 . property $ do
    i :: Int <- forAll $ Gen.integral (Range.linear minBound maxBound)
    i === bytesToInt (intToBytes i)
