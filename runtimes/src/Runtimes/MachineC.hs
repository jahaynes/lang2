{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Runtimes.MachineC where

import Phase.CodeGen.TypesC

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Vector ((!))
import qualified Data.Vector as V

interpret :: [CInstr ByteString] -> IO Int
interpret is = do
    
    let iv = V.fromList is

    let loop !ram !free !valStack !ipStack !regs !ip = do

            case iv ! ip of

                CComment{} ->
                    loop ram free valStack ipStack regs (ip+1)

                CLabel l -> do
                    putStrLn $ unpack l
                    loop ram free valStack ipStack regs (ip+1)

                CPush (CLitInt i) -> do
                    let valStack' = i:valStack
                    loop ram free valStack' ipStack regs (ip+1)

                CPush (CReg r) -> do
                    let Just v = M.lookup r regs
                    let valStack' = v:valStack
                    loop ram free valStack' ipStack regs (ip+1)

                CCall (CallReg r) -> do
                    let ipStack' = ip+1:ipStack
                    let Just ip' = M.lookup r regs
                    loop ram free valStack ipStack' regs ip'

                CCall (CallLabel f) -> do
                    let ipStack' = ip+1:ipStack
                    loop ram free valStack ipStack' regs (loc iv f)

                CCall (CallClosureAddr r) -> do
                    let Just p   = M.lookup r regs
                        Just v   = M.lookup p ram
                        ipStack' = ip+1:ipStack
                    loop ram free valStack ipStack' regs v

                CPop r -> do
                    let (v:valStack') = valStack
                        regs' = M.insert r v regs
                    loop ram free valStack' ipStack regs' (ip+1)

                CPlus d a b -> do
                    -- TODO this case-switching is probably not OK in machine code
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (a' + b') regs
                    loop ram free valStack ipStack regs' (ip+1)

                CMinus d a b -> do
                    -- TODO this case-switching is probably not OK in machine code
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (a' - b') regs
                    loop ram free valStack ipStack regs' (ip+1)

                CTimes d a b -> do
                    -- TODO this case-switching is probably not OK in machine code
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (a' * b') regs
                    loop ram free valStack ipStack regs' (ip+1)

                CAlloc r sz -> do
                    let (cells, 0) = sz `divMod` 8
                        free' = free + cells
                        regs' = M.insert r free regs
                    loop ram free' valStack ipStack regs' (ip+1)

                CMov (ToFrom d r) -> do
                    let Just val = M.lookup r regs
                    let regs' = M.insert d val regs
                    loop ram free valStack ipStack regs' (ip+1)

                CMov (ToOffsetFrom d o (CLbl f)) -> do
                    let Just p = M.lookup d regs
                        (cell, 0) = (p + o) `divMod` 8
                        ram' = M.insert cell (loc iv f) ram
                    loop ram' free valStack ipStack regs (ip+1)

                CMov (ToOffsetFrom d o (CReg r)) -> do
                    let Just p = M.lookup d regs
                        (cell, 0) = (p + o) `divMod` 8
                        Just v = M.lookup r regs
                        ram' = M.insert cell v ram
                    loop ram' free valStack ipStack regs (ip+1)
    
                CMov (ToFromOffset d r o) -> do
                    let Just p = M.lookup r regs
                        (cell, 0) = (p + o) `divMod` 8
                        Just v = M.lookup cell ram
                        regs' = M.insert d v regs
                    loop ram free valStack ipStack regs' (ip+1)

                CRet (CReg r) -> do
                    let Just v = M.lookup r regs
                    case ipStack of
                        [] -> pure v
                        (ip':stack') -> do
                            let valStack' = v:valStack
                            loop ram free valStack' stack' regs ip'

                x -> error $ show x

    loop M.empty 0 [] [] M.empty (loc iv "main")

    where
    loc iv f = fromJust $ V.findIndex isLabel iv
        where
        isLabel (CLabel l) = l == f
        isLabel          _ = False