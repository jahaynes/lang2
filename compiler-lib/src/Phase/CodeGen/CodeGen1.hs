{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen1 (codeGenModule1, renderCodeGen1) where

import Phase.CodeGen.CodeGen0

import           Data.ByteString      (ByteString)
import           Data.Map.Strict      ((!), Map)
import qualified Data.Map.Strict as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

data DSubRoutine s =
    DSubRoutine { dName           :: !s
                , dLocation       :: !Int
                , dInstructions   :: ![Instr s]
                , dStackPositions :: !(Map s Int)
                } deriving Show

renderCodeGen1 :: [Instr ByteString] -> Text
renderCodeGen1 = T.unlines
               . map (\(ln, instr) -> T.pack $ show ln ++ ":\t" ++ show instr)
               . zip [0..]

codeGenModule1 :: (Ord s, Show s) => [SubRoutine s] -> [Instr s]
codeGenModule1 = bakePos
               . analyze

bakePos :: (Ord s, Show s) => [DSubRoutine s] -> [Instr s]
bakePos dsubs =
    let subPos = M.fromList $ map (\ds -> (dName ds, dLocation ds)) dsubs
        baked  = goSub subPos <$> dsubs
        -- TODO this loses the stack positions
    in concatMap dInstructions baked
    where
    goSub subPos dSub =
        
        let is' = map goI (dInstructions dSub)
        in dSub { dInstructions = is' }
        where
        goI i =
            case i of
                Push v       -> Push (goV v)
                CallFun l    -> CallFun (goV l)
                Pop r        -> i -- ...
                Ret          -> i
                BinOpInstr{} -> i -- ...
                Assign dst v -> Assign dst (goV v)
                _            -> error $ show i

        goV v =
            case v of
                VInt{}   -> v
                Label l  -> VLoc (subPos ! l)
                Reg{}    -> v -- ...
                _        -> error $ show v

-- TODO - go all the way to general purpose registers now?
-- popping from the stack into the stack doesn't really make sense
-- maybe just ignore pops for now?  Need to unwind tho.  use sp, bp?
analyze :: (Ord s, Show s) => [SubRoutine s] -> [DSubRoutine s]
analyze = reverse . snd3 . foldr go (0, [], mempty)
    where
    snd3 (_, b, _) = b
    go (SubRoutine n is) (loc, acc, funLocs) =
        let spos = let rs = S.toList $ getRegistersIs is in M.fromList (zip rs [1..])
            dSub = DSubRoutine n loc is spos
            loc' = loc + length is
            acc' = dSub : acc
            funLocs' = M.insert n loc funLocs
        in (loc', acc', funLocs')

getRegistersIs :: (Ord s, Show s) => [Instr s] -> Set s
getRegistersIs = mconcat . map getRegistersI

getRegistersI :: (Ord s, Show s) => Instr s -> Set s
getRegistersI i =
    case i of
        Push v             -> getRegistersV v
        CallFun v          -> getRegistersV v
        Pop v              -> S.singleton v
        Ret{}              -> mempty
        BinOpInstr _ c a b -> S.insert c $ mconcat $ map getRegistersV [a, b]
        Assign c a         -> S.insert c $ getRegistersV a
        _                  -> error $ show i

getRegistersV :: (Ord s, Show s) => Val s -> Set s
getRegistersV v =
    case v of
        VInt{}  -> mempty
        Label{} -> mempty
        Reg r   -> S.singleton r
        _       -> error $ show v
