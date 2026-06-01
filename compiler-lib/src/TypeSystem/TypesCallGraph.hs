module TypeSystem.TypesCallGraph ( buildGraph
                                 , buildGraph'
                                 , planExcludingPretyped
                                 ) where

import Common.CallGraph
import Core.Expression
import Core.Module
import Core.Term

import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M
import           Data.Set              (Set, (\\))
import qualified Data.Set as S

-- TODO This only removes the depended-upon definitions.  Remove the dependers too?
planExcludingPretyped :: (Ord s, Show s) => Module t s -> CallGraph s -> Either ByteString [Set s]
planExcludingPretyped md (CallGraph cg) = do
    let pretyped = S.fromList . map (\(TypeSig n _) -> n) $ getTypeSigs md
    createPlan $ CallGraph (fmap (\\ pretyped) cg)

buildGraph :: (Ord s, Show s) => Module t s -> CallGraph s
buildGraph = buildGraph' . getFunDefns

-- TODO: pass in data definitions, and check for scope below in 'go'
buildGraph' :: (Ord s, Show s) => [FunDefn t s] -> CallGraph s
buildGraph' = CallGraph . M.unions . map go

    where
    go (FunDefn n _ e) = M.singleton n (fn (S.singleton n) e)

        where
        fn scope (Term _ (Var v))     = S.singleton v \\ scope
        fn     _ (Term _ (DCons d))   = mempty -- S.singleton d \\ scope -- Guess
        fn     _ (Term _       _)     = mempty
        fn scope (Lam _ vs body)      = fn (foldr S.insert scope vs) body
        fn scope (App _ f xs)         = mconcat $ map (fn scope) (f:xs)
        fn scope (Let _ a b c)        = let scope' = S.insert a scope in fn scope' b <> fn scope' c
        fn scope (UnPrimOp _ _ a)     = fn scope a
        fn scope (BinPrimOp _ _ a b)  = fn scope a <> fn scope b
        fn scope (IfThenElse _ p t f) = mconcat $ map (fn scope) [p, t, f]
        fn scope (Case _ scrut ps)    = fn scope scrut <> mconcat (map (pt scope) ps)

        pt scope (Pattern a b) = mempty -- TODO