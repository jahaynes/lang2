{-# LANGUAGE OverloadedLists,
             OverloadedStrings #-}

module TypeCheck.TypesCallGraphTest ( typesCallGraphTests ) where

import           Common.CallGraph          (CallGraph (..))
import           TypeSystem.TypesCallGraph (buildGraph')
import           Core.Expression
import           Core.Module
import           Core.Term
import           Core.Types (Untyped (..))

import           Hedgehog hiding (Var)

typesCallGraphTests :: Group
typesCallGraphTests =
    Group "callgraph" [ ("test_build_graph_empty", test_build_graph_empty)
                      , ("test_build_graph",       test_build_graph)
                      ]

test_build_graph_empty :: Property
test_build_graph_empty = unitTest $
    buildGraph' [] === (CallGraph mempty :: CallGraph ())

test_build_graph :: Property
test_build_graph = unitTest $ do

    let defn1 = FunDefn "foo" Unquant (App Untyped (Term Untyped (Var "bar")) [])
        defn2 = FunDefn "bar" Unquant (App Untyped (Term Untyped (Var "foo")) [])

    let CallGraph cg = buildGraph' [defn1, defn2] :: CallGraph String

    cg === [ ("bar", ["foo"])
           , ("foo", ["bar"]) ]

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p
