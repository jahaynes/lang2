{-# LANGUAGE OverloadedStrings #-}

module Common.TransTest (transTests) where

import           Common.EitherT
import           Common.Identity
import           Common.StateT
import           Common.Trans

import           Hedgehog

transTests :: Group
transTests =
    Group "trans" [ ("EitherT StateT doesn't roll back", test_eithert_statet_doesnt_rollback)
                  , ("StateT EitherT rolls back", test_statet_eithert_loses_state)
                  ]

test_statet_eithert_loses_state :: Property
test_statet_eithert_loses_state = unitTest $ do

    let steps = do
            successStep -- (+1)
            successStep -- (+1)
            failStep    -- (+1) then fail

    Left "Foo" === runIdentity (runEitherT (runStateT steps 0))

    where
    successStep :: StateT Int (EitherT String Identity) ()
    successStep = modifyt (+1)

    failStep :: StateT Int (EitherT String Identity) ()
    failStep = do
        modifyt (+1)
        lift $ left "Foo"

test_eithert_statet_doesnt_rollback :: Property
test_eithert_statet_doesnt_rollback = unitTest $ do

    let steps = do
            successStep -- (+1)
            successStep -- (+1)
            failStep    -- (+1) then fail

    (Left "Foo", 3) === runIdentity (runStateT (runEitherT steps) 0)

    where
    successStep :: EitherT String (StateT Int Identity) ()
    successStep = lift $ modifyt (+1)

    failStep :: EitherT String (StateT Int Identity) ()
    failStep = do
        lift $ modifyt (+1)
        left "Foo"

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p
