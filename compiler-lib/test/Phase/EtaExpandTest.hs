{-# LANGUAGE OverloadedLists,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Phase.EtaExpandTest (etaExpandTests) where

import Common.State
import Core.Expression
import Core.Module
import Core.Term
import Core.Types
import Phase.EtaExpand

import           Data.ByteString
import           Hedgehog hiding (Var)

etaExpandTests :: Group
etaExpandTests =
    Group "Eta Expansion" [] {- ("no_missing_args",  expandNoMissingArguments)
                          , ("one_missing_arg",  expandOneMissingArgument)
                          , ("two_missing_args", expandTwoMissingArguments)
                          ]

expandNoMissingArguments :: Property
expandNoMissingArguments =
    unitTest $
        evalState (expandDefn missing0) (EtaState 0) === missing0

expandOneMissingArgument :: Property
expandOneMissingArgument =
    unitTest $
        evalState (expandDefn missing1) (EtaState 1) === missing0

expandTwoMissingArguments :: Property
expandTwoMissingArguments =
    unitTest $
        evalState (expandDefn missing2) (EtaState 0) === missing0

(->>) :: Type s -> Type s -> Type s
(->>) = TyArr

missing0 :: TFunDefn ByteString
missing0 =
    TFunDefn "missing" (ALam (Forall [] (typeBool ->> (typeInt ->> typeString)))
                             ["eta_0", "eta_1"]
                             (AApp (Forall [] typeString)
                                   (ATerm (Forall [] (typeBool ->> (typeInt ->> typeString)))
                                          (Var "full")) [ ATerm (Forall [] typeBool) (Var "eta_0")
                                                        , ATerm (Forall [] typeInt)  (Var "eta_1") 
                                                        ]))

missing1 :: TFunDefn ByteString
missing1 =
    TFunDefn "missing" (ALam (Forall [] (typeBool ->> (typeInt ->> typeString)))
                             ["eta_0"]
                             (AApp (Forall [] (typeInt ->> typeString))
                                   (ATerm (Forall [] (typeBool ->> (typeInt ->> typeString)))
                                          (Var "full")) [ ATerm (Forall [] typeBool) (Var "eta_0") ]))

missing2 :: TFunDefn ByteString
missing2 =
    TFunDefn "missing" (ATerm (Forall [] (typeBool ->> (typeInt ->> typeString))) (Var "full"))
-}
unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property

