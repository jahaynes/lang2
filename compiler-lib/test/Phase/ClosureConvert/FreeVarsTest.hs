{-# LANGUAGE OverloadedStrings #-}

module Phase.ClosureConvert.FreeVarsTest where

import           Core.Operator
import           Core.Term
import           Phase.Anf.AnfExpression
import           Phase.ClosureConvert.FreeVars

import qualified Data.Set as S
import           Hedgehog hiding (Var)

freeVarsTests :: Group
freeVarsTests =
    Group "freeVarsTests" [ ("no_free_vars", test_no_free_vars)
                          , ("test_lambda_free_vars", test_lambda_free_vars)
                          ]

test_no_free_vars :: Property
test_no_free_vars = unitTest $ do

    let inp = NLet "a"
                   (AExp (ATerm (LitInt 1)))
                   (AExp (ATerm (Var "a")))

    getFreeVars mempty inp === mempty

test_lambda_free_vars :: Property
test_lambda_free_vars = unitTest $ do

    let inp = AExp $ ALam ["a"] $ AExp $ ABinPrimOp AddI
                                                    (ATerm $ Var "a")
                                                    (ATerm $ Var "b")

    getFreeVars mempty inp === S.singleton "b"

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p
