{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedLists
           , OverloadedStrings #-}

module Phase.Unclobber.UnclobberRecursiveRegisters (unclobberRecursiveRegisters) where

import Common.EitherT
import Common.State
import Common.Trans
import Phase.CodeGen.TypesC

import           Control.Monad           (zipWithM_)
import           Data.ByteString.Char8   (ByteString, pack)
import           Data.Map                (Map)
import qualified Data.Map as M
import           Data.Set                ((\\), Set)
import qualified Data.Set as S

type Unclobber s a =
    EitherT ByteString
        (State (Liveness s)) a

{-
    Using virtual registers everywhere prevents clobbering,
    except in the case of recursive calls, which are forced
    to clobber registers.

    This fixes that with caller-save pushes and pops.

    FIXME: It currently preserves too many registers (registers
    which are not overwritten in the call)
-}
unclobberRecursiveRegisters :: [[CInstr ByteString]] -> Either ByteString [[CInstr ByteString]]
unclobberRecursiveRegisters = mapM unclobber

unclobber :: [CInstr ByteString] -> Either ByteString [CInstr ByteString]
unclobber is =

    let state = Liveness mempty mempty mempty mempty mempty

    in case runState (runEitherT $ findClobbered is) state of

        (Left err, _) ->
            Left err

        (Right{}, live) ->
            Right $ go (callBlocks live)
                0
                (M.toAscList $ clobbered live) -- Need to be applied in order (to keep line-numbers correct)
                is

    where
    go :: Map (Int, ByteString) (Int, Int)
       -> Int
       -> [((Int, ByteString), Set R)]
       -> [CInstr ByteString]
       -> [CInstr ByteString]
    go    _   _                     [] is = is
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

findClobbered :: (Show s, Ord s) => [CInstr s] -> Unclobber s ()
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
    go _ (CDiv   d a b) = goBinOp d a b
    go _ (CMod   d a b) = goBinOp d a b
    go _ (CEq    d a b) = goBinOp d a b
    go _ (CAnd   d a b) = goBinOp d a b
    go _ (COr    d a b) = goBinOp d a b
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
        left "Blind call.  Must preserve all regs?"

    go _ CRet{} =
        pure ()

    go _ (CAlloc d _) =
        regWrite [d]

    go _ (CNeg r) = do
        regRead [r]
        regWrite [r]

    go _ x =
        left $ "Unhandled case: " <> pack (show x)

goBinOp :: Ord s => R -> CVal s -> CVal s -> Unclobber s ()
goBinOp d a b = do
    regWrite [d]
    regRead (regOf a <> regOf b)

goMov :: Ord s => MovMode s -> Unclobber s ()
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

regWrite :: Set R -> Unclobber s ()
regWrite regs = lift . modify' $ \live ->

           -- Update the current write set state
    live { writeSet = let WriteSet ws = writeSet live in WriteSet (regs <> ws)

           -- We unmark registers as clobbered if they are written after the call but before a read
         , liveness = (\(WriteSet ws, rs) -> (WriteSet (ws \\ regs), rs)) <$> liveness live }

regRead :: Ord s => Set R -> Unclobber s ()
regRead regs = lift . modify' $ \live ->
    let ReadSet rs = readSet live
    in live { readSet   = ReadSet (regs <> rs)
            , clobbered = updateClobbered (liveness live) (clobbered live)
            }
    where
    updateClobbered lv = M.unionWith S.union (M.map (\(WriteSet ws, _) -> S.intersection ws regs) lv)

call :: Ord s => Int -> s -> Int -> Int -> Unclobber s ()
call lineNo lbl prePushes postPops = lift . modify' $ \live ->
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
