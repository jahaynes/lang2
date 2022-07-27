{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.TypeCheckTest where

import Common.State
import Core.Expression
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import TypeCheck.TypeCheckTypes
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
                PartialInference ity ics <- infer $ ETerm (LitInt 0)
                PartialInference bty bcs <- infer $ ETerm (LitBool True)
                PartialInference sty scs <- infer $ ETerm (LitString "str")
                pure (annot ity, annot bty, annot sty, ics ++ bcs ++ scs)

    r === (TyCon "Int", TyCon "Bool", TyCon "String", [])

test_generalisations :: Property
test_generalisations = unitTest $ do

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ FunDefn "fst" (ELam ["a","b"] (ETerm (Var "a")))
                                     , FunDefn "snd" (ELam ["x","y"] (ETerm (Var "y"))) ]
                    }

    let Right (TypedModule tFunDefns) =
            inferModule md (TypeCheckPlan [S.singleton "fst", S.singleton "snd"])

    map getNameAndType tFunDefns === [ ("snd", Forall ["b", "c"] (TyVar "b" `TyArr` (TyVar "c" `TyArr` TyVar "c")))
                                     , ("fst", Forall ["b", "c"] (TyVar "b" `TyArr` (TyVar "c" `TyArr` TyVar "b"))) ]

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

    let Right (TypedModule tFunDefns) =
            inferModule md (TypeCheckPlan [S.singleton "countDown"])

    map getNameAndType tFunDefns === [("countDown", Forall [] (TyArr (TyCon "Int") (TyCon "String")))]

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

    let Right (TypedModule tFunDefns) =
            inferModule md (TypeCheckPlan [S.singleton "summorial"])

    map getNameAndType tFunDefns === [("summorial", Forall [] (TyArr (TyCon "Int") (TyCon "Int")))]

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


    let Right (TypedModule tFunDefns) =
            inferModule md (TypeCheckPlan [S.fromList ["yep", "yesnt"]])

    map getNameAndType tFunDefns === [ ( "yep",   Forall ["f"] (TyArr (TyVar "f") (TyCon "Bool")) )
                                     , ( "yesnt", Forall ["f"] (TyArr (TyVar "f") (TyCon "Bool")) ) ]

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property

getNameAndType :: TFunDefn s -> (s, Polytype s)
getNameAndType (TFunDefn name ex) = (name, annot ex)
