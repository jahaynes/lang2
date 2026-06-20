{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedLists
           , OverloadedStrings #-}

module Phase.Unclobber.UnclobberRecursiveRegisters (unclobberRecursiveRegisters) where

import Common.State
import Core.Types
import Phase.CodeGen.TypesA

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
unclobberRecursiveRegisters :: [[AInstr ByteString]] -> Either ByteString [[AInstr ByteString]]
unclobberRecursiveRegisters = Right . map unclobber

unclobber :: [AInstr ByteString] -> [AInstr ByteString]
unclobber is =

    let liveness =
            execState (findClobbered is) (Liveness mempty mempty mempty mempty mempty mempty)

    in go (typeInfo liveness)
          (callBlocks liveness)
          0
          (M.toAscList $ clobbered liveness) -- Need to be applied in order (to keep line-numbers correct)
          is

    where
    go :: Map Int (Type ByteString) -- TODO neaten
       -> Map (Int, ByteString) (Int, Int)
       -> Int
       -> [((Int, ByteString), Set Int)]
       -> [AInstr ByteString]
       -> [AInstr ByteString]
    go typeInf rngs off                     [] is = is
    go typeInf rngs off (((ln, lbl), regs):cs) is =
        let Just (lo, hi)    = M.lookup (ln, lbl) rngs
            (startMid, rest) = splitAt (hi + 1 - off) is
            (start, mid)     = splitAt (lo - off) startMid
            off' = hi + 1
        in concat ( [ start
                    , map createPush (S.toList regs)
                    , mid
                    , map createPop  (reverse $ S.toList regs)
                    , go typeInf rngs off' cs rest ] :: [[AInstr ByteString]])
        where
        createPush :: Int -> AInstr ByteString
        createPush r = let Just t = M.lookup r typeInf in Push "unclobber" t (AReg r)

        createPop :: Int -> AInstr ByteString
        createPop r = let Just t = M.lookup r typeInf in Pop "unclobber" t r

newtype WriteSet =
    WriteSet (Set Int)
        deriving (Semigroup, Monoid, Show)

newtype ReadSet =
    ReadSet (Set Int)
        deriving (Semigroup, Monoid, Show)

data Liveness s =
    Liveness { writeSet   :: !WriteSet
             , readSet    :: !ReadSet
             , liveness   :: !(Map (Int, s) (WriteSet, ReadSet))
             , clobbered  :: !(Map (Int, s) (Set Int))
             , typeInfo   :: !(Map Int (Type s))
             , callBlocks :: !(Map (Int, s) (Int, Int))
             } deriving Show

findClobbered :: Ord s => [AInstr s] -> State (Liveness s) ()
findClobbered = zipWithM_ go [0..]

    where
    go _ ALabel{} =
        pure ()

    go _ (Pop _ t w) = do
        preserveTypeInfo w t
        regWrite [w]

    go _ (Push _ _ r) =
        regRead (regOf r)

    go _ (ABinOp w _ t a _ b) = do
        preserveTypeInfo w t
        regWrite [w]
        regRead (regOf a <> regOf b)

    go _ (ACmpB r) =
        regRead (regOf r)

    go _ J{} =
        pure ()

    go _ Jne{} =
        pure ()

    go _ (AMov t mov) =
        goMov t mov

    go n (Call lbl prePushes postPops) =
        call n lbl prePushes postPops

    go _ Ret{} =
        pure ()

goMov :: Ord s => Type s -> MovMode -> State (Liveness s) ()
goMov t (RegFromReg w r) = do
    preserveTypeInfo w t
    regWrite [w]
    regRead [r]
goMov t (RegFromLitInt w _) = do
    preserveTypeInfo w t
    regWrite [w]
goMov _ x = error . show $ ("urr: movmode", x)

preserveTypeInfo :: Int -> Type s -> State (Liveness s) ()
preserveTypeInfo r t = modify' $ \live ->
    live { typeInfo = M.insert r t (typeInfo live) }

regWrite :: Set Int -> State (Liveness s) ()
regWrite regs = modify' $ \live ->

           -- Update the current write set state
    live { writeSet = let WriteSet ws = writeSet live in WriteSet (regs <> ws)

           -- We unmark registers as clobbered if they are written after the call but before a read
         , liveness = (\(WriteSet ws, rs) -> (WriteSet (ws \\ regs), rs)) <$> liveness live }

regRead :: Ord s => Set Int -> State (Liveness s) ()
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

regOf :: AVal -> Set Int
regOf (AReg r) = [r]
regOf        _ = []
