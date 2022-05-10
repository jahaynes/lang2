{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.CallGraph where

import Core.Definition
import Core.Expression
import Core.Term

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.List             (partition)
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

buildGraph :: (Ord s, Show s) => [FunDefn s] -> Map s (Set s)
buildGraph fundefns = M.unions $ map go fundefns

    where
    go (FunDefn n e) = M.singleton n (fn (S.singleton n) e) -- does including n in scope here hide mutual recursions?

        where
        fn scope (ETerm (Var v))    = S.singleton v \\ scope
        fn     _ (ETerm       _)    = mempty
        fn scope (ELam vs body)     = fn (foldr S.insert scope vs) body
        fn scope (EApp f xs)        = mconcat $ map (fn scope) (f:xs)
        fn scope (ELet a b c)       = let scope' = S.insert a scope in fn scope' b <> fn scope' c
        fn scope (EUnPrimOp _ a)    = fn scope a
        fn scope (EBinPrimOp _ a b) = fn scope a <> fn scope b
        fn scope (IfThenElse p t f) = mconcat $ map (fn scope) [p, t, f]
        fn     _ EClo{}             = error "EClo"
        fn     _ CallClo{}          = error "CallClo"

-- Cleanup
-- use a Seq too
findCycles :: Ord a => Map a (Set a) -> Set (Set a)
findCycles graph = S.map S.fromList $ S.unions $ map (S.fromList . go []) $ M.keys graph

    where
    go path b
        | b `elem` path = [dropWhile (/= b) path]
        | otherwise =
            case M.lookup b graph of
                Nothing -> []
                Just outs ->
                    let path' = path ++ [b]
                    in concatMap (go path') (S.toList outs)

-- only exists to be tested?
mergeIntersections :: Ord a => [Set a] -> [Set a]
mergeIntersections       [] = []
mergeIntersections (xs:xss) =
    let (disjoint, joint) = partition (S.null . S.intersection xs) xss
    in mconcat (xs:joint) : mergeIntersections disjoint


findLeaves :: Ord a => Map a a -> Set a
findLeaves graph =
    let outs = S.fromList $ M.elems graph
        ins  = M.keysSet graph
    in outs \\ ins

plan :: [FunDefn ByteString] -> Either ByteString [Set ByteString]
plan = solve . buildGraph

solve :: Map ByteString (Set ByteString) -> Either ByteString [Set ByteString]
solve = go []
    where
    go solved graph
        | null graph = Right $ reverse solved
        | otherwise =
            let allSolved = S.unions solved
                (soluble, insoluble) = M.partition (all (`S.member` allSolved)) graph
            in if null soluble
                then do
                    let cycles = S.toList $ findCycles graph
                    next <- findStandaloneCycle insoluble cycles
                    let graph' = foldr M.delete graph next
                    go (next : solved) graph'
                else do
                    let graph' = graph `M.difference` soluble
                    go (M.keysSet soluble:solved) graph'

findStandaloneCycle :: Map ByteString (Set ByteString) -> [Set ByteString] -> Either ByteString (Set ByteString)
findStandaloneCycle graph = go []
    where
    go xs            [] = Left $ "Could not find standalone cycle among: " <> pack (show xs)
    go before (c:after) =
        let deps = mconcat $ map (graph !) (S.toList c)
        in if all (\b -> null (b `S.intersection` deps)) before && all (\a -> null (a `S.intersection` deps)) after
            then Right c
            else go (c:before) after
