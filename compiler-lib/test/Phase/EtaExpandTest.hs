{-# LANGUAGE OverloadedLists,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Phase.EtaExpandTest (etaExpandTests) where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Phase.EtaExpand.EtaExpand

import           Data.ByteString
import           Hedgehog hiding (Var)

etaExpandTests :: Group
etaExpandTests =
    Group "Eta Expansion" [ ("no_missing_args",  expandNoMissingArguments)
                          , ("one_missing_arg",  expandOneMissingArgument)
                          , ("two_missing_args", expandTwoMissingArguments)
                          ]

expandNoMissingArguments :: Property
expandNoMissingArguments =
    unitTest $
        evalState (expandDefn missing0) (EtaState 0 mempty mempty) === missing0

expandOneMissingArgument :: Property
expandOneMissingArgument =
    unitTest $
        evalState (expandDefn missing1) (EtaState 1 mempty mempty) === missing0

expandTwoMissingArguments :: Property
expandTwoMissingArguments =
    unitTest $
        evalState (expandDefn missing2) (EtaState 0 mempty mempty) === missing0

-- TODO dedupe
(->>) :: Type s -> Type s -> Type s
(->>) = TyArr

missing0 :: FunDefnT (Type ByteString) ByteString
missing0 =
    FunDefnT "missing" (Quant [])
                       (Lam (typeBool ->> (typeInt ->> typeString))
                             ["eta_0", "eta_1"]
                             (App typeString
                                   (Term (typeBool ->> (typeInt ->> typeString)) (Var "full"))
                                   [ Term typeBool (Var "eta_0")
                                   , Term typeInt  (Var "eta_1") ]))

missing1 :: FunDefnT (Type ByteString) ByteString
missing1 =
    FunDefnT "missing" (Quant [])
                       (Lam (typeBool ->> (typeInt ->> typeString))
                             ["eta_0"]
                             (App (typeInt ->> typeString)
                                   (Term (typeBool ->> (typeInt ->> typeString)) (Var "full"))
                                   [ Term typeBool (Var "eta_0") ]))

missing2 :: FunDefnT (Type ByteString) ByteString
missing2 =
    FunDefnT "missing" (Quant []) (Term (typeBool ->> (typeInt ->> typeString)) (Var "full"))

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property