{-# LANGUAGE OverloadedStrings #-}

module Runtimes.Machine0 (runMachine0) where

import Common.State
import Phase.CodeGen.CodeGen0

import           Data.ByteString      (ByteString)
import           Data.Map.Strict      ((!), Map)
import qualified Data.Map.Strict as M
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

runMachine0 :: [SubRoutine ByteString] -> ByteString
runMachine0 srs = do

    error . unlines . map show $ analyze srs
{-
    let program = Program
                . M.fromList
                . map (\(SubRoutine n is) -> (n, is))
                $ srs

    let d = runState (runProgram program "main") (MachineState [] mempty)

    error $ show d
-}

data DSubRoutine s =
    DSubRoutine { dLocation     :: !Int
                , dInstructions :: ![Instr s]
                , dStackSize    :: !Int
                } deriving Show

analyze :: [SubRoutine s] -> [DSubRoutine s]
analyze = reverse . snd . foldr go (0, [], mempty)
    where
    go (SubRoutine n is) (loc, acc, funLocs) =
        let dSub     = DSubRoutine loc is (-1) 
            loc'     = loc + length is
            acc'     = dSub : acc
            funLocs' = funLocs
        in (loc', acc', funLocs')

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