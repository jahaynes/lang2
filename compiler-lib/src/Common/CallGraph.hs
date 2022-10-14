{-# LANGUAGE OverloadedStrings #-}

module Common.CallGraph where

import Core.Expression
import Core.Module
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule

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

-- TODO: pass in data definitions, and check for scope below in 'go'
buildGraph' :: (Ord s, Show s) => [FunDefn s] -> CallGraph s
buildGraph' = CallGraph . M.unions . map go

    where
    go (FunDefn n e) = M.singleton n (fn (S.singleton n) e)

        where
        fn scope (ETerm (Var v))    = S.singleton v \\ scope
        fn     _ (ETerm (DCons d))  = mempty -- S.singleton d \\ scope -- Guess
        fn     _ (ETerm       _)    = mempty
        fn scope (ELam vs body)     = fn (foldr S.insert scope vs) body
        fn scope (EApp f xs)        = mconcat $ map (fn scope) (f:xs)
        fn scope (ELet a b c)       = let scope' = S.insert a scope in fn scope' b <> fn scope' c
        fn scope (EUnPrimOp _ a)    = fn scope a
        fn scope (EBinPrimOp _ a b) = fn scope a <> fn scope b
        fn scope (IfThenElse p t f) = mconcat $ map (fn scope) [p, t, f]
        fn scope (ECase scrut ps)   = fn scope scrut <> mconcat (map (pt scope) ps)

        pt scope (Pattern a b) = mempty -- TODO

buildGraphAnf :: (Ord s, Show s) => AnfModule s -> CallGraph s
buildGraphAnf = buildGraphAnf' . getFunDefAnfTs

buildGraphAnf' :: (Ord s, Show s) => [FunDefAnfT s] -> CallGraph s
buildGraphAnf' fundefns = CallGraph . M.unions $ map go fundefns

    where
    go (FunDefAnfT n _ e) = M.singleton n (fn e (S.singleton n))

        where
        fn (AExp aexp)  = fna aexp
        fn (CExp cexp)  = fnc cexp
        fn (NLet a b c) = fnl a b c

        fna aexp scope =
            case aexp of
                ATerm _ (Var v)    -> S.singleton v \\ scope
                ATerm _ DCons{}    -> error "dcons"
                ATerm _       _    -> mempty
                ALam _ vs body     -> fn body (foldr S.insert scope vs)
                AClo _ fvs vs body -> fn body (foldr S.insert scope (fvs++vs))
                ABinPrimOp _ a b   -> fna a scope <> fna b scope

        fnc cexp scope =
            case cexp of
                CApp f xs            -> mconcat $ map (`fna` scope) (f:xs)
                CIfThenElse pr tr fl -> mconcat [ fna pr scope, fn tr scope, fn fl scope]

        fnl a b c scope =
            let scope' = S.insert a scope
            in fn b scope' <> fn c scope'

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
    createPlan $ CallGraph (fmap (\\ pretyped) cg)

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

findStandaloneCycle :: (Ord s, Show s) => Map s (Set s) -> [Set s] -> Either ByteString (Set s)
findStandaloneCycle graph = go []
    where
    go xs            [] = Left $ "Could not find standalone cycle among: " <> pack (show xs)
    go before (c:after) =
        let deps = mconcat $ map (graph !) (S.toList c)
        in if all (\b -> null (b `S.intersection` deps)) before && all (\a -> null (a `S.intersection` deps)) after
            then Right c
            else go (c:before) after
