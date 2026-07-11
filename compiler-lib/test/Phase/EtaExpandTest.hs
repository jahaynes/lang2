{-# LANGUAGE OverloadedLists,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Phase.EtaExpandTest (etaExpandTests) where

import Common.State
import Core.Expression
import Core.Module
import Core.Operator
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
                          , ("lambda_body",      expandLambdaBody)
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

missing0 :: FunDefn (Type ByteString) ByteString
missing0 =
    FunDefn "missing" (Quant [])
                      (Lam (typeBool ->> (typeInt ->> typeString))
                           ["eta_0", "eta_1"]
                           (App typeString
                                (Term (typeBool ->> (typeInt ->> typeString)) (Var "full"))
                                [ Term typeBool (Var "eta_0")
                                , Term typeInt  (Var "eta_1") ]))

missing1 :: FunDefn (Type ByteString) ByteString
missing1 =
    FunDefn "missing" (Quant [])
                      (Lam (typeBool ->> (typeInt ->> typeString))
                           ["eta_0"]
                           (App (typeInt ->> typeString)
                                (Term (typeBool ->> (typeInt ->> typeString)) (Var "full"))
                                [ Term typeBool (Var "eta_0") ]))

missing2 :: FunDefn (Type ByteString) ByteString
missing2 =
    FunDefn "missing" (Quant []) (Term (typeBool ->> (typeInt ->> typeString)) (Var "full"))

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property

expandLambdaBody :: Property
expandLambdaBody =
    unitTest $ do
        -- Exercise the full production pipeline (etaExpand → expandDefn → etaSaturate)
        -- to ensure nested-lambda merging doesn't poison getExtraParams
        let md = Module { getDataDefns = []
                        , getTypeSigs  = []
                        , getFunDefns  = [fDefn, mainDefn]
                        }
        let result = etaExpand md
        let funDefns = getFunDefns result
        -- f: nested lambdas should be merged, no crash from missing "y" in known types
        funDefns !! 0 === fDefnExpanded
        -- main: fully applied, unchanged
        funDefns !! 1 === mainDefn

-- f :: Int -> Int -> Int
-- f x = \y. y + x
fDefn :: FunDefn (Type ByteString) ByteString
fDefn =
    FunDefn "f" (Quant []) $
        Lam (int ->> (int ->> int)) ["x"] $
            Lam (int ->> int) ["y"] $
                BinPrimOp int AddI (Term int (Var "y")) (Term int (Var "x"))

-- f after eta expansion (nested lambdas get merged into one)
fDefnExpanded :: FunDefn (Type ByteString) ByteString
fDefnExpanded =
    FunDefn "f" (Quant []) $
        Lam (int ->> (int ->> int)) ["x", "y"] $
            BinPrimOp int AddI (Term int (Var "y")) (Term int (Var "x"))

-- main :: Int
-- main = (f 2) 3
mainDefn :: FunDefn (Type ByteString) ByteString
mainDefn =
    FunDefn "main" (Quant []) $
        App int
            (App (int ->> int)
                 (Term (int ->> (int ->> int)) (Var "f"))
                 [Term int (LitInt 2)])
            [Term int (LitInt 3)]

int :: Type ByteString
int = TyCon "Int" []