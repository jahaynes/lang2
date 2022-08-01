{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.CallGraph (Cycle, Graph (..), buildGraph, findCycles, plan, renderCycles, renderGraph) where

import Core.Definition
import Core.Expression
import Core.Term

import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as C8
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

newtype Graph s =
    Graph (Map s (Set s))
        deriving (Eq, Show)

instance Ord s => Semigroup (Graph s) where
    Graph a <> Graph b = Graph (a <> b)

instance Ord s => Monoid (Graph s) where
    mempty = Graph mempty

newtype Cycle s =
    Cycle (Set s)

renderGraph :: Graph ByteString -> ByteString
renderGraph (Graph graph) = C8.unlines
                          . map renderLine
                          $ M.toList graph
    where
    renderLine (n, deps) = C8.unwords [n, "->", "[" <> (C8.intercalate "," $ S.toList deps) <> "]"]

renderCycles :: [Cycle ByteString] -> ByteString
renderCycles = C8.unlines . map (\(Cycle s) -> pack . show $ S.toList s)

buildGraph :: (Ord s, Show s) => [FunDefn s] -> Graph s
buildGraph = Graph . M.unions . map go

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
findCycles :: Ord a => Graph a -> Set (Set a)
findCycles (Graph graph) = S.map S.fromList
                         . S.unions
                         . map (S.fromList . go [])
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

plan :: (Ord s, Show s) => [FunDefn s]
                        -> Either ByteString [Cycle s]
plan = solve . buildGraph

solve :: (Ord s, Show s) => Graph s
                         -> Either ByteString [Cycle s]
solve (Graph g) = go [] g
    where
    go solved graph
        | null graph = Right (Cycle <$> reverse solved)
        | otherwise =
            let allSolved = S.unions solved
                (soluble, insoluble) = M.partition (all (`S.member` allSolved)) graph
            in if null soluble
                then do
                    let cycles = S.toList $ findCycles (Graph graph)
                    Cycle next <- findStandaloneCycle (Graph insoluble) cycles
                    let graph' = foldr M.delete graph next
                    go (next : solved) graph'
                else do
                    let graph' = graph `M.difference` soluble
                    go (M.keysSet soluble:solved) graph'

findStandaloneCycle :: (Ord s, Show s) => Graph s
                                       -> [Set s]
                                       -> Either ByteString (Cycle s)
findStandaloneCycle (Graph graph) = go []
    where
    go xs            [] = Left $ "Could not find standalone cycle among: " <> pack (show xs)
    go before (c:after) =
        let deps = mconcat $ map (graph !) (S.toList c)
        in if all (\b -> null (b `S.intersection` deps)) before && all (\a -> null (a `S.intersection` deps)) after
            then Right (Cycle c)
            else go (c:before) after
