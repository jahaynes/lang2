{-# LANGUAGE OverloadedStrings #-}

module Runtimes.Machine0 (codeGen1) where

import Common.State
import Phase.CodeGen.CodeGen0

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Map.Strict      ((!), Map)
import qualified Data.Map.Strict as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Debug.Trace          (trace)

newtype Ptr =
    Ptr Int

data Machine =
    Machine { allocate :: !(Int -> Ptr) }

newtype Program s =
    Program (Map s [Instr s])

data MachineState s =
    MachineState { getStack      :: ![Val s]
                 , getStackFrame :: !(Map s (Val s))
                 } deriving Show

-- TODO line numbers

codeGen1 :: [SubRoutine ByteString] -> ByteString
codeGen1 = C8.unlines
         . map (C8.pack . show)
         . bakePos
         . analyze
{-
    let program = Program
                . M.fromList
                . map (\(SubRoutine n is) -> (n, is))
                $ srs

    let d = runState (runProgram program "main") (MachineState [] mempty)

    error $ show d
-}

data DSubRoutine s =
    DSubRoutine { dName           :: !s
                , dLocation       :: !Int
                , dInstructions   :: ![Instr s]
                , dStackPositions :: !(Map s Int)
                } deriving Show

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
        _                  -> error $ show i

getRegistersV :: (Ord s, Show s) => Val s -> Set s
getRegistersV v =
    case v of
        VInt{}  -> mempty
        Label{} -> mempty
        Reg r   -> S.singleton r
        _       -> error $ show v

{-

runProgram :: (Ord s, Show s) => Program s -> s -> State (MachineState s) ()
runProgram (Program p) entry = goLbl entry

    where
    goLbl lbl = 
        let is = p ! lbl in trace (show is) (goIs is)

    goIs     [] = error "No more instructions"
    goIs (i:is) =

        case i of
        
            Push val -> do
                push val
                goIs is
            
            CallFun (Label lbl) ->
                goLbl lbl
                -- 

            Pop reg -> do
                pop reg
                goIs is

            BinOpInstr op dst (Reg a) (Reg b) -> do
                a' <- getReg a
                b' <- getReg b
                undefined



            _ -> error $ show i

getReg reg = do
    sf <- getStackFrame <$> get
    pure $ sf ! reg

push :: Val s -> State (MachineState s) ()
push val =
    modify' $ \ms ->
        ms { getStack = val : getStack ms }

pop :: Ord s => s -> State (MachineState s) ()
pop reg =
    modify' $ \ms ->
        let s:tack      = getStack ms
            stackFrame' = M.insert reg s (getStackFrame ms)
        in ms { getStack      = tack 
              , getStackFrame = stackFrame'
              }

-}