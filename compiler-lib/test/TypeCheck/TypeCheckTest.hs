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
    Group "Typecheck" [ ("primitives",             test_primitives)
                      , ("generalisations",        test_generalisations)
                      , ("top_level_recursion",    test_top_level_recursion)
                      , ("nested_recursion",       test_nested_recursion)
                      , ("test_mutual_recursion",  test_mutual_recursion)
                      , ("datatypes",              test_simple_datatype)
                      , ("recursive_datatypes",    test_recursive_datatype)
                      , ("lambda_body",            test_lambda_body)
                      , ("pattern_match_datatype", test_pattern_matching)
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
                    , getFunDefns  = [ FunDefn "fst" Unquant (Lam Untyped ["a","b"] (Term Untyped (Var "a")))
                                     , FunDefn "snd" Unquant (Lam Untyped ["x","y"] (Term Untyped (Var "y"))) ]
                    }

    let r = map getPolyType . getFunDefns <$> inferModule md

    r === Right [ Forall ["a0", "b0"] (TyVar "a0" ->> (TyVar "b0" ->> TyVar "a0"))
                , Forall ["a0", "b0"] (TyVar "a0" ->> (TyVar "b0" ->> TyVar "b0")) ]

test_top_level_recursion :: Property
test_top_level_recursion = unitTest $ do

    let fundefn =
          FunDefn "countDown" Unquant
                  (Lam Untyped ["n"] (IfThenElse Untyped (BinPrimOp Untyped EqA (Term Untyped (Var "n")) (Term Untyped (LitInt 0)))
                                                           (Term Untyped (LitString "Done"))
                                                           (App Untyped (Term Untyped (Var "countDown")) [BinPrimOp Untyped SubI
                                                                                                           (Term Untyped (Var "n"))
                                                                                                           (Term Untyped (LitInt 1))])))
    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ fundefn ] }

    let r = map getPolyType . getFunDefns <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> TyCon "String" [])]

test_nested_recursion :: Property
test_nested_recursion = unitTest $ do

    let fundefn =
          FunDefn "summorial" Unquant
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

    let r = map getPolyType . getFunDefns <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> TyCon "Int" [])]

test_mutual_recursion :: Property
test_mutual_recursion = unitTest $ do

    let yep =
          FunDefn "yep" Unquant $
              Lam Untyped ["y"] $
                  App Untyped (Term Untyped (Var "not")) [App Untyped (Term Untyped (Var "yesnt")) [Term Untyped (Var "y")]]

    let yesnt =
          FunDefn "yesnt" Unquant $
              Lam Untyped ["n"] $
                  App Untyped (Term Untyped (Var "not")) [App Untyped (Term Untyped (Var "yep")) [Term Untyped (Var "n")]]

    let md = Module { getDataDefns = []
                    , getTypeSigs  = [ TypeSig "not" (TyCon "Bool" [] ->> TyCon "Bool" []) ]
                    , getFunDefns  = [ yep, yesnt ] }

    let r = map getPolyType . getFunDefns <$> inferModule md

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
    let fNo  = FunDefn "no"  Unquant (App Untyped (Term Untyped $ DCons "No")
                                                  [Term Untyped $ LitString "no"])
        fYes = FunDefn "yes" Unquant (App Untyped (Term Untyped $ DCons "Yes")
                                                  [Term Untyped $ LitInt 1])

    let md = Module { getDataDefns = [answer]
                    , getTypeSigs  = []
                    , getFunDefns  = [fNo, fYes]
                    }

    let Right inferredModule =
            inferModule md :: Either ByteString (Module (Type ByteString) ByteString)

    let inferredFunTypes =
            map getPolyType $ getFunDefns inferredModule

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
    let myList  = FunDefn "myList" Unquant (App Untyped (Term Untyped $ DCons "Cons") [ Term Untyped $ LitInt 1
                                                                                      , Term Untyped $ DCons "Empty"])

    let md = Module { getDataDefns = [list]
                    , getTypeSigs  = []
                    , getFunDefns  = [myList]
                    }

    let Right inferredModule =
            inferModule md :: Either ByteString (Module (Type ByteString) ByteString)

    let inferredFunTypes =
            map getPolyType $ getFunDefns inferredModule

    inferredFunTypes === [Forall [] (TyCon "List" [TyCon "Int" []])]

{-
    f x = \y. y + x
-}
test_lambda_body :: Property
test_lambda_body = unitTest $ do

    let fundefn =
          FunDefn "f" Unquant $
              Lam Untyped ["x"] $
                  Lam Untyped ["y"] $
                      BinPrimOp Untyped AddI (Term Untyped (Var "y")) (Term Untyped (Var "x"))

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ fundefn ] }

    let r = map getPolyType . getFunDefns <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> (TyCon "Int" [] ->> TyCon "Int" []))]

{-
    Pair a b = MkPair a b

    main =
        let snd pair =
                case pair of
                    MkPair a b -> b in
        snd (MkPair 1 2)
-}
test_pattern_matching :: Property
test_pattern_matching = unitTest $ do

    -- Datatype: Pair a b = MkPair a b
    let dcMkPair = DataCon "MkPair" [MemberVar "a", MemberVar "b"]
        pair     = DataDefn "Pair" ["a", "b"] [dcMkPair]

    -- main = let snd pair = case pair of MkPair a b -> b in snd (MkPair 1 2)
    let mainExpr =
            Let Untyped "snd"
                (Lam Untyped ["pair"]
                    (Case Untyped (Term Untyped (Var "pair"))
                        [ Pattern (PDCons Untyped "MkPair" [PVar Untyped "a", PVar Untyped "b"])
                                  (Term Untyped (Var "b")) ]))
                (App Untyped (Term Untyped (Var "snd"))
                    [ App Untyped (Term Untyped (DCons "MkPair"))
                                 [ Term Untyped (LitInt 1)
                                 , Term Untyped (LitInt 2) ]])

    let md = Module { getDataDefns = [pair]
                    , getTypeSigs  = []
                    , getFunDefns  = [FunDefn "main" Unquant mainExpr]
                    }

    let result = inferModule md :: Either ByteString (Module (Type ByteString) ByteString)

    case result of
        Left err -> do
            footnote $ "Error: " <> show err
            failure
        Right inferredModule -> do
            let inferredFunTypes =
                    map getPolyType $ getFunDefns inferredModule
            inferredFunTypes === [Forall [] (TyCon "Int" [])]

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property

getPolyType :: FunDefn (Type s) s -> Polytype s
getPolyType (FunDefn _ (Quant vs) expr) = Forall vs (typeOf expr)

(->>) :: Type s -> Type s -> Type s
(->>) = TyArr
