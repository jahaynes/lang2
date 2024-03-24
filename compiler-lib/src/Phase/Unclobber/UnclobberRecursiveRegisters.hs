{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedLists
           , OverloadedStrings #-}

module Phase.Unclobber.UnclobberRecursiveRegisters (unclobberRecursiveRegisters) where

import Common.State
import Phase.CodeGen.TypesC

import           Control.Monad           (zipWithM_)
import           Data.ByteString         (ByteString)
import           Data.Map                (Map)
import qualified Data.Map as M
import           Data.Set                ((\\), Set)
import qualified Data.Set as S

{-
    Using virtual registers everywhere prevents clobbering,
    except in the case of recursive calls, which are forced
    to clobber registers.

    This fixes that with caller-save pushes and pops.

    FIXME: It currently preserves too many registers (registers
    which are not overwritten in the call)
-}
unclobberRecursiveRegisters :: [[CInstr ByteString]] -> Either ByteString [[CInstr ByteString]]
unclobberRecursiveRegisters = Right . map unclobber

unclobber :: [CInstr ByteString] -> [CInstr ByteString]
unclobber is =

    let liveness =
            execState (findClobbered is) (Liveness mempty mempty mempty mempty mempty)

    in go (callBlocks liveness)
          0
          (M.toAscList $ clobbered liveness) -- Need to be applied in order (to keep line-numbers correct)
          is

    where
    go :: Map (Int, ByteString) (Int, Int)
       -> Int
       -> [((Int, ByteString), Set R)]
       -> [CInstr ByteString]
       -> [CInstr ByteString]
    go rngs off                     [] is = is
    go rngs off (((ln, lbl), regs):cs) is =
        let Just (lo, hi)    = M.lookup (ln, lbl) rngs
            (startMid, rest) = splitAt (hi + 1 - off) is
            (start, mid)     = splitAt (lo - off) startMid
            off' = hi + 1
        in concat ( [ start
                    , map (CPush . CReg) (S.toList regs)
                    , mid
                    , map CPop (reverse $ S.toList regs)
                    , go rngs off' cs rest ] :: [[CInstr ByteString]])

newtype WriteSet =
    WriteSet (Set R)
        deriving (Semigroup, Monoid, Show)

newtype ReadSet =
    ReadSet (Set R)
        deriving (Semigroup, Monoid, Show)

data Liveness s =
    Liveness { writeSet   :: !WriteSet
             , readSet    :: !ReadSet
             , liveness   :: !(Map (Int, s) (WriteSet, ReadSet))
             , clobbered  :: !(Map (Int, s) (Set R))
             , callBlocks :: !(Map (Int, s) (Int, Int))
             } deriving Show

findClobbered :: (Show s, Ord s) => [CInstr s] -> State (Liveness s) ()
findClobbered = zipWithM_ go [0..]

    where
    go _ CLabel{} =
        pure ()

    go _ (CPop w) =
        regWrite [w]

    go _ (CPush r) =
        regRead (regOf r)

    go _ (CPlus  d a b) = goBinOp d a b
    go _ (CMinus d a b) = goBinOp d a b
    go _ (CTimes d a b) = goBinOp d a b
    go _ (CEq    d a b) = goBinOp d a b
    go _ (CAnd   d a b) = goBinOp d a b
    go _ (CLt    d a b) = goBinOp d a b

    go _ (CCmpB r) =
        regRead [r]

    go _ J{} =
        pure ()

    go _ Jne{} =
        pure ()

    go _ (CMov mov) =
        goMov mov

    -- TODO - this needs to work on non-label call targets.  See below
    go n (CCall (CallLabel lbl) prePushes postPops) =
        call n lbl prePushes postPops

    -- This is a blind call
    go n (CCall CallReg{} prePushes postPops) =
        error "Blind call.  Must preserve all regs?"

    go _ CRet{} =
        pure ()

    go _ (CAlloc d _) =
        regWrite [d]

    go _ x = error $ show x

goBinOp d a b = do
    regWrite [d]
    regRead (regOf a <> regOf b)

goMov :: (Show s, Ord s) => MovMode s -> State (Liveness s) ()
goMov (ToFrom w r) = do
    regWrite [w]
    regRead [r]
goMov (FromLitInt w _) =
    regWrite [w]
goMov (ToOffsetFrom d _ a) = do
    regWrite [d]
    regRead (regOf a)
goMov (ToFromOffset d a _) = do
    regWrite [d]
    regRead [a]

regWrite :: Set R -> State (Liveness s) ()
regWrite regs = modify' $ \live ->

           -- Update the current write set state
    live { writeSet = let WriteSet ws = writeSet live in WriteSet (regs <> ws)

           -- We unmark registers as clobbered if they are written after the call but before a read
         , liveness = (\(WriteSet ws, rs) -> (WriteSet (ws \\ regs), rs)) <$> liveness live }

regRead :: Ord s => Set R -> State (Liveness s) ()
regRead regs = modify' $ \live ->
    let ReadSet rs = readSet live
    in live { readSet   = ReadSet (regs <> rs)
            , clobbered = updateClobbered (liveness live) (clobbered live)
            }
    where
    updateClobbered lv = M.unionWith S.union (M.map (\(WriteSet ws, _) -> S.intersection ws regs) lv)

call :: Ord s => Int -> s -> Int -> Int -> State (Liveness s) ()
call lineNo lbl prePushes postPops = modify' $ \live ->
    live { liveness = M.insert (lineNo, lbl)
                               (writeSet live, readSet live)
                               (liveness live)
         , callBlocks = M.insert (lineNo, lbl)
                                 (lineNo - prePushes, lineNo + postPops)
                                 (callBlocks live)
         }

regOf :: CVal s -> Set R
regOf (CReg r) = [r]
regOf        _ = []
