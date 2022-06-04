{-# LANGUAGE OverloadedLists,
             OverloadedStrings,
             ScopedTypeVariables #-}

module TypeCheck.CallGraphTest (callGraphTests) where

import           Core.Definition
import           Core.Expression
import           Core.Term
import           TypeCheck.CallGraph

import           Data.Map (Map)
import           Data.Set (Set)
import           Hedgehog hiding (Var)

callGraphTests :: Group
callGraphTests =
    Group "callgraph" [ ("test_empty",    test_empty)
                      , ("test_disjoint", test_disjoint)
                      , ("test_joint",    test_joint)
                      , ("test_both",     test_both)

                      , ("test_build_graph_empty",  test_build_graph_empty)
                      , ("test_build_graph",        test_build_graph)

                      , ("test_empty_cycles",    test_empty_cycles)
                      , ("test_no_cycles",       test_no_cycles)
                      , ("test_one_cycle",       test_one_cycle)
                      , ("test_mixed_cycles",    test_mixed_cycles)
                      , ("test_disjoint_cycles", test_disjoint_cycles)
                      ]

test_empty :: Property
test_empty = unitTest $
    let empty = [] :: [Set Int]
    in mergeIntersections empty === empty

test_disjoint :: Property
test_disjoint = unitTest $
    mergeIntersections [[1::Int,2], [3,4]] === [[1,2], [3,4]]

test_joint :: Property
test_joint = unitTest $
    mergeIntersections [[1::Int,2], [2,4]] === [[1,2,4]]

test_both :: Property
test_both = unitTest $
    mergeIntersections [[1::Int,2], [3,5], [2,4]] === [[1,2,4], [3,5]]



test_build_graph_empty :: Property
test_build_graph_empty = unitTest $
    buildGraph [] === (mempty :: Map () (Set ()))

test_build_graph :: Property
test_build_graph = unitTest $ do

    let defn1 = FunDefn "foo" (EApp (ETerm (Var "bar")) [])
        defn2 = FunDefn "bar" (EApp (ETerm (Var "foo")) [])

    buildGraph [defn1, defn2] === ([ ("bar", ["foo"])
                                   , ("foo", ["bar"]) ] :: Map String (Set String))



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
