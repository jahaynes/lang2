{-# LANGUAGE OverloadedStrings #-}

module Common.CallGraph ( CallGraph (..)
                        , createPlan
                        , findCycles -- just for testing
                        ) where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S

newtype CallGraph s =
    CallGraph (Map s (Set s))
        deriving (Eq, Show)

createPlan :: (Ord s, Show s) => CallGraph s -> Either ByteString [Set s]
createPlan (CallGraph cg) = go [] cg
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

findStandaloneCycle :: (Ord s, Show s) => Map s (Set s) -> [Set s] -> Either ByteString (Set s)
findStandaloneCycle graph = go []
    where
    go xs            [] = Left $ "Could not find standalone cycle among: " <> pack (show xs)
    go before (c:after) =
        let deps = mconcat $ map (graph !) (S.toList c)
        in if all (\b -> null (b `S.intersection` deps)) before && all (\a -> null (a `S.intersection` deps)) after
            then Right c
            else go (c:before) after
