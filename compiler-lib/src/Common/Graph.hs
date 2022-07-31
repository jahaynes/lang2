{-# LANGUAGE OverloadedStrings #-}

module Common.Graph where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S

newtype Graph a =
    Graph (Map a (Set a))
        deriving (Eq, Show)

newtype Cycles a =
    Cycles (Set (Set a))


-- Cleanup
-- use a Seq too
findCycles :: Ord a => Graph a -> Cycles a
findCycles (Graph graph) = Cycles
                         . S.map S.fromList
                         . S.unions 
                         . map (S.fromList . go mempty)
                         $ M.keys graph

    where
    go path b
        | b `elem` path = [dropWhile (/= b) path]
        | otherwise =
            case M.lookup b graph of
                Nothing -> []
                Just outs ->
                    let path' = path ++ [b]
                    in concatMap (go path') (S.toList outs)

findStandaloneCycle :: (Ord s, Show s) => Map s (Set s)
                                       -> [Set s]
                                       -> Either ByteString (Set s)
findStandaloneCycle graph = go []
    where
    go xs            [] = Left $ "Could not find standalone cycle among: " <> pack (show xs)
    go before (c:after) =
        let deps = mconcat $ map (graph !) (S.toList c)
        in
        if all (\b -> null (b `S.intersection` deps)) before
        && all (\a -> null (a `S.intersection` deps)) after
            then Right c
            else go (c:before) after
