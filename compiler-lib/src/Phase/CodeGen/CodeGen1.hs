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
codeGenModule1 = concatMap foo
    where
    foo sub = ILabel (getName sub) : getInstrs sub

{- TODO:
    Either bake in the tr_ and fl_ branches as part of this
    or take out baking completely and refer to it as 'linkerInfo' downstream
-}

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

        where
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
                Cmp v              -> getRegistersV v
                JmpLbl{}           -> mempty
                JmpNeqLbl{}        -> mempty
                ILabel{}           -> mempty
                _                  -> error $ show i

        getRegistersV :: (Ord s, Show s) => Val s -> Set s
        getRegistersV v =
            case v of
                VInt{}  -> mempty
                Label{} -> mempty
                Reg r   -> S.singleton r
                _       -> error $ show v
