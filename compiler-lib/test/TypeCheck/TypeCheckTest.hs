{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.TypeCheckTest where

import Common.State
import Core.Expression
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import TypeCheck.TypeInference

import qualified Data.Set as S
import           Hedgehog hiding (Var)

typeCheckTests :: Group
typeCheckTests =
    Group "Typecheck" [ ("primitives", test_primitives)
                      , ("generalisations", test_generalisations)
                      , ("top_level_recursion", test_top_level_recursion)
                      , ("nested_recursion", test_nested_recursion)
                      , ("test_mutual_recursion", test_mutual_recursion)
                      ]

test_primitives :: Property
test_primitives = unitTest $ do

    let st = InferState 0 mempty (PolytypeEnv mempty)

    let r = fst $ runState' st $ do
                (_, ity, ics) <- infer $ ETerm (LitInt 0)
                (_, bty, bcs) <- infer $ ETerm (LitBool True)
                (_, sty, scs) <- infer $ ETerm (LitString "str")
                pure (ity, bty, sty, ics ++ bcs ++ scs)

    r === (TyCon "Int", TyCon "Bool", TyCon "String", [])

test_generalisations :: Property
test_generalisations = unitTest $ do

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ FunDefn "fst" (ELam ["a","b"] (ETerm (Var "a")))
                                     , FunDefn "snd" (ELam ["x","y"] (ETerm (Var "y"))) ]
                    }

    let Right (TypedModule tfunDefns) =
            inferModule md [S.fromList ["fst", "snd"]]

    length tfunDefns  === 2

    let [fstType] = map (\(TFunDefn t _ _) -> t)
                  . filter (\(TFunDefn _ n _) -> n == "fst")
                  $ tfunDefns

    fstType === Forall ["a", "b"] (TyArr (TyVar "a") (TyArr (TyVar "b") (TyVar "a")))

    let [sndType] = map (\(TFunDefn t _ _) -> t)
                  . filter (\(TFunDefn _ n _) -> n == "snd")
                  $ tfunDefns

    sndType === Forall ["a", "b"] (TyArr (TyVar "a") (TyArr (TyVar "b") (TyVar "b")))

test_top_level_recursion :: Property
test_top_level_recursion = unitTest $ do

    let fundefn =
          FunDefn "countDown"
                  (ELam ["n"] (IfThenElse (EBinPrimOp EqA (ETerm (Var "n")) (ETerm (LitInt 0)))
                                          (ETerm (LitString "Done"))
                                          (EApp (ETerm (Var "countDown")) [EBinPrimOp SubI
                                                                                      (ETerm (Var "n"))
                                                                                      (ETerm (LitInt 1))])))
    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ fundefn ] }

    let Right (TypedModule tfunDefns) =
            inferModule md [S.singleton "countDown"]

    let [typ] = map (\(TFunDefn t _ _) -> t) tfunDefns

    typ === Forall [] (TyArr (TyCon "Int") (TyCon "String"))

test_nested_recursion :: Property
test_nested_recursion = unitTest $ do

    let fundefn =
          FunDefn "summorial"
                  (ELam ["n"]
                        (ELet "go"
                              (ELam ["acc","m"]
                                    (IfThenElse (EBinPrimOp EqA (ETerm (Var "m")) (ETerm (LitInt 0)))
                                                (ETerm (Var "acc"))
                                                (EApp (ETerm (Var "go")) [ EBinPrimOp AddI (ETerm (Var "acc")) (ETerm (Var "m"))
                                                                         , EBinPrimOp SubI (ETerm (Var "m")) (ETerm (LitInt 1)) ])))
                              (EApp (ETerm (Var "go")) [ETerm (LitInt 0),ETerm (Var "n")])))

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ fundefn ] }

    let Right (TypedModule tfunDefns) =
            inferModule md [S.singleton "summorial"]

    let [typ] = map (\(TFunDefn t _ _) -> t) tfunDefns

    typ === Forall [] (TyArr (TyCon "Int") (TyCon "Int"))

test_mutual_recursion :: Property
test_mutual_recursion = unitTest $ do

    let yep =
          FunDefn "yep" $
              ELam ["y"] $
                  EApp (ETerm (Var "not")) [EApp (ETerm (Var "yesnt")) [ETerm (Var "y")]]

    let yesnt =
          FunDefn "yesnt" $
              ELam ["n"] $
                  EApp (ETerm (Var "not")) [EApp (ETerm (Var "yep")) [ETerm (Var "n")]]

    let md = Module { getDataDefns = []
                    , getTypeSigs  = [ TypeSig "not" (TyCon "Bool" `TyArr` TyCon "Bool") ]
                    , getFunDefns  = [ yep, yesnt ] }

    let Right (TypedModule tfunDefns) =
            inferModule md [S.fromList ["yep", "yesnt"]]

    let types = map (\(TFunDefn t _ _) -> t) tfunDefns

    types === [ Forall ["a"] (TyVar "a" `TyArr` TyCon "Bool")
              , Forall ["a"] (TyVar "a" `TyArr` TyCon "Bool")
              ]

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property
