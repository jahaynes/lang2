{-# LANGUAGE OverloadedLists,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Common.CallGraphTest (callGraphTests) where

import           Common.CallGraph
import           Core.Expression
import           Core.Module
import           Core.Term
import           Core.Types (Untyped (..))

import           Data.Map (Map)
import           Data.Set (Set)
import           Hedgehog hiding (Var)

callGraphTests :: Group
callGraphTests =
    Group "callgraph" [ ("test_build_graph_empty",  test_build_graph_empty)
                      , ("test_build_graph",        test_build_graph)

                      , ("test_empty_cycles",    test_empty_cycles)
                      , ("test_no_cycles",       test_no_cycles)
                      , ("test_one_cycle",       test_one_cycle)
                      , ("test_mixed_cycles",    test_mixed_cycles)
                      , ("test_disjoint_cycles", test_disjoint_cycles)
                      ]

test_build_graph_empty :: Property
test_build_graph_empty = unitTest $
    buildGraph' [] === (CallGraph mempty :: CallGraph ())

test_build_graph :: Property
test_build_graph = unitTest $ do

    let defn1 = FunDefn "foo" (App Untyped (Term Untyped (Var "bar")) [])
        defn2 = FunDefn "bar" (App Untyped (Term Untyped (Var "foo")) [])

    let CallGraph cg = buildGraph' [defn1, defn2] :: CallGraph String

    cg === [ ("bar", ["foo"])
           , ("foo", ["bar"]) ]


test_empty_cycles :: Property
test_empty_cycles = unitTest $
    let input = mempty :: Map () (Set ())
    in findCycles input === mempty

test_no_cycles :: Property
test_no_cycles = unitTest $
    let input = [ ('a', ['b'])
                , ('c', ['d']) ]
    in findCycles input === mempty

test_one_cycle :: Property
test_one_cycle = unitTest $
    let input = [ ('a', ['b'])
                , ('b', ['a']) ]
    in findCycles input === [ ['a', 'b'] ]

test_mixed_cycles :: Property
test_mixed_cycles = unitTest $
    let input = [ ('a', ['b'])
                , ('b', ['a', 'c']) ]
    in findCycles input === [ ['a', 'b'] ]

test_disjoint_cycles :: Property
test_disjoint_cycles = unitTest $
    let input = [ ('*', ['1', 'a'])
                , ('1', ['2'])
                , ('2', ['1'])
                , ('a', ['b'])
                , ('b', ['a']) ]
    in findCycles input === [ ['a', 'b'], ['1', '2'] ]



unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p
