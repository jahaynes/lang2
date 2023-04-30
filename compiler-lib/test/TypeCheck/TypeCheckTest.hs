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
    Group "Typecheck" [ --("primitives",            test_primitives)
                      --, ("generalisations",       test_generalisations)
                      --, ("top_level_recursion",   test_top_level_recursion)
                      --, ("nested_recursion",      test_nested_recursion)
                      --, ("test_mutual_recursion", test_mutual_recursion)
                        ("datatypes", test_simple_datatype)
                      ]

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
    let fNo  = FunDefn "no"  (EApp (ETerm $ DCons "No")
                                   [ETerm $ LitString "no"])
        fYes = FunDefn "yes" (EApp (ETerm $ DCons "Yes")
                                   [ETerm $ LitInt 1])

    let md = Module { getDataDefns = [answer]
                    , getTypeSigs  = []
                    , getFunDefns  = [fNo, fYes]
                    }

    let Right inferredModule =
            inferModule md :: Either ByteString (ModuleT ByteString)

    let inferredFunTypes =
            map getPolyType $ getFunDefnTs inferredModule

    inferredFunTypes === [ Forall [] (TyCon "Answer" [TyCon "String" []])
                         , Forall [] (TyCon "Answer" [TyCon "Int" []])
                         ]

test_primitives :: Property
test_primitives = unitTest $ do

    let r =
            evalState' undefined $
                mapM (inferTerm mempty) [ LitInt 33
                                        , LitBool True
                                        , LitString "str" ]

    r === [ TermT (TyCon "Int" [])    (LitInt 33)
          , TermT (TyCon "Bool" [])   (LitBool True)
          , TermT (TyCon "String" []) (LitString "str") ]

test_generalisations :: Property
test_generalisations = unitTest $ do

    let md = Module { getDataDefns = []
                    , getTypeSigs  = []
                    , getFunDefns  = [ FunDefn "fst" (ELam ["a","b"] (ETerm (Var "a")))
                                     , FunDefn "snd" (ELam ["x","y"] (ETerm (Var "y"))) ]
                    }

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [ Forall ["a0", "b0"] (TyVar "a0" ->> (TyVar "b0" ->> TyVar "a0"))
                , Forall ["a0", "b0"] (TyVar "a0" ->> (TyVar "b0" ->> TyVar "b0")) ]

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

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> TyCon "String" [])]

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

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [Forall [] (TyCon "Int" [] ->> TyCon "Int" [])]

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
                    , getTypeSigs  = [ TypeSig "not" (TyCon "Bool" [] ->> TyCon "Bool" []) ]
                    , getFunDefns  = [ yep, yesnt ] }

    let r = map getPolyType . getFunDefnTs <$> inferModule md

    r === Right [ Forall ["a0"] (TyVar "a0" ->> TyCon "Bool" [])
                , Forall ["a0"] (TyVar "a0" ->> TyCon "Bool" []) ]

unitTest :: PropertyT IO () -> Property
unitTest = withTests 1 . property

getPolyType :: FunDefnT s -> Polytype s
getPolyType (FunDefnT _ (Quant vs) exprT) = Forall vs (typeOf exprT)

(->>) :: Type s -> Type s -> Type s
(->>) = TyArr
