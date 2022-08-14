{-# LANGUAGE OverloadedStrings #-}

module Common.CallGraph where

import Core.Expression
import Core.Module
import Core.Term

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map              (Map, (!))
import qualified Data.Map as M
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

newtype CallGraph s =
    CallGraph (Map s (Set s))
        deriving (Eq, Show)

buildGraph :: (Ord s, Show s) => Module s -> CallGraph s
buildGraph = buildGraph' . getFunDefns

buildGraph' :: (Ord s, Show s) => [FunDefn s] -> CallGraph s
buildGraph' fundefns = CallGraph . M.unions $ map go fundefns

    where
    go (FunDefn n e) = M.singleton n (fn (S.singleton n) e)

        where
        fn scope (ETerm (Var v))    = S.singleton v \\ scope
        fn     _ (ETerm       _)    = mempty
        fn scope (ELam vs body)     = fn (foldr S.insert scope vs) body
        fn scope (EApp f xs)        = mconcat $ map (fn scope) (f:xs)
        fn scope (ELet a b c)       = let scope' = S.insert a scope in fn scope' b <> fn scope' c
        fn scope (EUnPrimOp _ a)    = fn scope a
        fn scope (EBinPrimOp _ a b) = fn scope a <> fn scope b
        fn scope (IfThenElse p t f) = mconcat $ map (fn scope) [p, t, f]

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

-- TODO This only removes the depended-upon definitions.  Remove the dependers too?
planExcludingPretyped :: (Ord s, Show s) => Module s -> CallGraph s -> Either ByteString [Set s]
planExcludingPretyped md (CallGraph cg) = do
    let pretyped = S.fromList . map (\(TypeSig n _) -> n) $ getTypeSigs md
    plan $ CallGraph (fmap (\\ pretyped) cg)

plan :: (Ord s, Show s) => CallGraph s -> Either ByteString [Set s]
plan (CallGraph cg) = go [] cg
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

findStandaloneCycle :: (Ord s, Show s) => Map s (Set s) -> [Set s] -> Either ByteString (Set s)
findStandaloneCycle graph = go []
    where
    go xs            [] = Left $ "Could not find standalone cycle among: " <> pack (show xs)
    go before (c:after) =
        let deps = mconcat $ map (graph !) (S.toList c)
        in if all (\b -> null (b `S.intersection` deps)) before && all (\a -> null (a `S.intersection` deps)) after
            then Right c
            else go (c:before) after
