{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.TypeCheckTest (typeCheckTests) where

import Common.State
import Core.Expression
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import TypeSystem.InferTerm
import TypeSystem.TypeCheck

import Data.ByteString (ByteString)
import Hedgehog hiding (Var)

typeCheckTests :: Group
typeCheckTests =
    Group "Typecheck" [ ("primitives",            test_primitives)
                      , ("generalisations",       test_generalisations)
                      , ("top_level_recursion",   test_top_level_recursion)
                      , ("nested_recursion",      test_nested_recursion)
                      , ("test_mutual_recursion", test_mutual_recursion)
                      , ("datatypes",             test_simple_datatype)
                      , ("recursive_datatypes",   test_recursive_datatype)
                      ]



test_primitives :: Property
test_primitives = unitTest $ do

    let r =
            evalState' undefined $
                mapM (inferTerm mempty) [ LitInt 33
                                        , LitBool True
                                        , LitString "str" ]

    r === [ Term (TyCon "Int" [])    (LitInt 33)
          , Term (TyCon "Bool" [])   (LitBool True)
          , Term (TyCon "String" []) (LitString "str") ]

test_generalisations :: Property
test_generalisations = unitTest $ do

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ FunDefn "fst" (Lam Untyped ["a","b"] (Term Untyped (Var "a")))
                                     , FunDefn "snd" (Lam Untyped ["x","y"] (Term Untyped (Var "y"))) ]
                    }

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [ Forall ["a0", "b0"] (TyVar "a0" ->> (TyVar "b0" ->> TyVar "a0"))
                , Forall ["a0", "b0"] (TyVar "a0" ->> (TyVar "b0" ->> TyVar "b0")) ]

test_top_level_recursion :: Property
test_top_level_recursion = unitTest $ do

    let fundefn =
          FunDefn "countDown"
                  (Lam Untyped ["n"] (IfThenElse Untyped (BinPrimOp Untyped EqA (Term Untyped (Var "n")) (Term Untyped (LitInt 0)))
                                                           (Term Untyped (LitString "Done"))
                                                           (App Untyped (Term Untyped (Var "countDown")) [BinPrimOp Untyped SubI
                                                                                                           (Term Untyped (Var "n"))
                                                                                                           (Term Untyped (LitInt 1))])))
    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ fundefn ] }

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> TyCon "String" [])]

test_nested_recursion :: Property
test_nested_recursion = unitTest $ do

    let fundefn =
          FunDefn "summorial"
                  (Lam Untyped ["n"]
                        (Let Untyped "go"
                              (Lam Untyped ["acc","m"]
                                    (IfThenElse Untyped (BinPrimOp Untyped EqA (Term Untyped (Var "m")) (Term Untyped (LitInt 0)))
                                                         (Term Untyped (Var "acc"))
                                                         (App Untyped (Term Untyped (Var "go")) [ BinPrimOp Untyped AddI (Term Untyped (Var "acc")) (Term Untyped (Var "m"))
                                                                                                  , BinPrimOp Untyped SubI (Term Untyped (Var "m")) (Term Untyped (LitInt 1)) ])))
                              (App Untyped (Term Untyped (Var "go")) [ Term Untyped (LitInt 0)
                                                                       , Term Untyped (Var "n")])))

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ fundefn ] }

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> TyCon "Int" [])]

test_mutual_recursion :: Property
test_mutual_recursion = unitTest $ do

    let yep =
          FunDefn "yep" $
              Lam Untyped ["y"] $
                  App Untyped (Term Untyped (Var "not")) [App Untyped (Term Untyped (Var "yesnt")) [Term Untyped (Var "y")]]

    let yesnt =
          FunDefn "yesnt" $
              Lam Untyped ["n"] $
                  App Untyped (Term Untyped (Var "not")) [App Untyped (Term Untyped (Var "yep")) [Term Untyped (Var "n")]]

    let md = Module { getDataDefns = []
                    , getTypeSigs  = [ TypeSig "not" (TyCon "Bool" [] ->> TyCon "Bool" []) ]
                    , getFunDefns  = [ yep, yesnt ] }

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [ Forall ["a0"] (TyVar "a0" ->> TyCon "Bool" [])
                , Forall ["a0"] (TyVar "a0" ->> TyCon "Bool" []) ]

{-
    Answer a = Yes a | No a
    yes = Yes 1
    no = No "no"
-}
test_simple_datatype :: Property
test_simple_datatype = unitTest $ do

    -- Datatype
    let dcYes  = DataCon     "Yes" [MemberVar "a"]
        dcNo   = DataCon      "No" [MemberVar "a"]
        answer = DataDefn "Answer" ["a"] [dcYes, dcNo]

    -- Functions
    let fNo  = FunDefn "no"  (App Untyped (Term Untyped $ DCons "No")
                                           [Term Untyped $ LitString "no"])
        fYes = FunDefn "yes" (App Untyped (Term Untyped $ DCons "Yes")
                                           [Term Untyped $ LitInt 1])

    let md = Module { getDataDefns = [answer]
                    , getTypeSigs  = []
                    , getFunDefns  = [fNo, fYes]
                    }

    let Right inferredModule =
            inferModule md :: Either ByteString (ModuleT (Type ByteString) ByteString)

    let inferredFunTypes =
            map getPolyType $ getFunDefnTs inferredModule

    inferredFunTypes === [ Forall [] (TyCon "Answer" [TyCon "String" []])
                         , Forall [] (TyCon "Answer" [TyCon "Int" []])
                         ]

{-
    List a = Empty | Cons a (List a)
    myList = Cons 1 Empty
-}
test_recursive_datatype :: Property
test_recursive_datatype = unitTest $ do

    -- Datatype
    let dcEmpty = DataCon  "Empty" []
        dcCons  = DataCon  "Cons" [MemberVar "a", MemberType "List" [MemberVar "a"]]
        list    = DataDefn "List" ["a"] [dcEmpty, dcCons]

    -- Functions
    let myList  = FunDefn "myList" (App Untyped (Term Untyped $ DCons "Cons") [ Term Untyped $ LitInt 1
                                                                                , Term Untyped $ DCons "Empty"])

    let md = Module { getDataDefns = [list]
                    , getTypeSigs  = []
                    , getFunDefns  = [myList]
                    }

    let Right inferredModule =
            inferModule md :: Either ByteString (ModuleT (Type ByteString) ByteString)

    let inferredFunTypes =
            map getPolyType $ getFunDefnTs inferredModule

    inferredFunTypes === [Forall [] (TyCon "List" [TyCon "Int" []])]

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property

getPolyType :: FunDefnT (Type s) s -> Polytype s
getPolyType (FunDefnT _ (Quant vs) expr) = Forall vs (typeOf expr)

(->>) :: Type s -> Type s -> Type s
(->>) = TyArr
