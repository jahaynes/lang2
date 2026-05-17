{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Runtimes.MachineC where

import Phase.CodeGen.TypesC

import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Vector ((!))
import qualified Data.Vector as V

interpret :: [CInstr ByteString] -> IO Int
interpret is = do
    
    let iv = V.fromList is

    let loop !ram !free !valStack !ipStack !regs !cmp !ip = do

            case iv ! ip of

                CComment{} ->
                    loop ram free valStack ipStack regs cmp (ip+1)

                CLabel{} ->
                    loop ram free valStack ipStack regs cmp (ip+1)

                CPush x ->
                    cPush x

                CCall call _ _ ->
                    cCall call

                CPop r ->
                    let (v:valStack') = valStack
                        regs' = M.insert r v regs
                    in loop ram free valStack' ipStack regs' cmp (ip+1)

                CPlus d a b ->
                    cBinOp d a b (+)

                CMinus d a b ->
                    cBinOp d a b (-)

                CTimes d a b ->
                    cBinOp d a b (*)

                CDiv d a b ->
                    cBinOp d a b div

                CMod d a b ->
                    cBinOp d a b mod

                CLt d a b ->
                    cBinOp d a b (\a' b' -> if a'  < b' then 1 else 0)

                CEq d a b ->
                    -- TODO potentially unsound if comparing two bools which aren't 1/0
                    cBinOp d a b (\a' b' -> if a' == b' then 1 else 0)

                CAnd d a b ->
                    cBinOp d a b (\a' b' -> if a' /= 0 && b' /= 0 then 1 else 0)

                COr d a b ->
                    cBinOp d a b (\a' b' -> if a' /= 0 || b' /= 0 then 1 else 0)

                CNeg r ->
                    let Just val = M.lookup r regs
                        regs' = M.insert r (-val) regs
                    in loop ram free valStack ipStack regs' cmp (ip+1)

                CAlloc r sz ->
                    let (cells, 0) = sz `divMod` 8
                        free' = free + cells
                        regs' = M.insert r free regs
                    in loop ram free' valStack ipStack regs' cmp (ip+1)

                CCmpB r ->
                    let Just val = M.lookup r regs
                        cmp' = not (val == 0)
                    in loop ram free valStack ipStack regs cmp' (ip+1)

                J lbl ->
                    loop ram free valStack ipStack regs cmp (loc iv lbl)

                Je{} -> error "TODO"

                Jne lbl ->
                    let ip' = if cmp then ip+1 else loc iv lbl
                    in loop ram free valStack ipStack regs cmp ip'

                CMov mov ->
                    cMov mov

                CRet (CReg r) -> do
                    let Just v = M.lookup r regs
                    case ipStack of
                        [] -> pure v
                        (ip':stack') -> do
                            let valStack' = v:valStack
                            loop ram free valStack' stack' regs cmp ip'

                CRet (CLitInt i) ->
                    case ipStack of
                        [] -> pure i
                        (ip':stack') -> do
                            let valStack' = i:valStack
                            loop ram free valStack' stack' regs cmp ip'

                CRet (CLbl{}) -> error "TODO"

                CErr{} -> error "TOdO"

                where
                cPush x = do
                    let Just v = case x of
                                     CLitInt i -> Just i
                                     CReg r    -> M.lookup r regs
                    let valStack' = v:valStack
                    loop ram free valStack' ipStack regs cmp (ip+1)

                cBinOp d a b f = do
                    -- TODO this case-switching is probably not OK in machine code
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (f a' b') regs
                    loop ram free valStack ipStack regs' cmp (ip+1)

                cCall call =

                    case call of

                        CallReg r ->
                            let ipStack' = ip+1:ipStack
                                Just ip' = M.lookup r regs
                            in loop ram free valStack ipStack' regs cmp ip'

                        CallLabel f ->
                            let ipStack' = ip+1:ipStack
                            in loop ram free valStack ipStack' regs cmp (loc iv f)

                        CallClosureAddr r ->
                            let Just p   = M.lookup r regs
                                Just v   = M.lookup p ram
                                ipStack' = ip+1:ipStack
                            in loop ram free valStack ipStack' regs cmp v

                cMov mov =

                    case mov of

                        FromLitInt d i ->
                            fromLitInt d i

                        ToFrom d r ->
                            toFrom d r

                        ToOffsetFrom d o (CLbl f) ->
                            let Just p = M.lookup d regs
                                (cell, 0) = (p + o) `divMod` 8
                                ram' = M.insert cell (loc iv f) ram
                            in loop ram' free valStack ipStack regs cmp (ip+1)

                        ToOffsetFrom d o (CReg r) ->
                            let Just p = M.lookup d regs
                                (cell, 0) = (p + o) `divMod` 8
                                Just v = M.lookup r regs
                                ram' = M.insert cell v ram
                            in loop ram' free valStack ipStack regs cmp (ip+1)

                        ToOffsetFrom d o (CLitInt i) ->
                            let Just p = M.lookup d regs
                                (cell, 0) = (p + o) `divMod` 8
                                ram' = M.insert cell i ram
                            in loop ram' free valStack ipStack regs cmp (ip+1)

                        ToFromOffset d r o ->
                            let Just p = M.lookup r regs
                                (cell, 0) = (p + o) `divMod` 8
                                Just v = M.lookup cell ram
                                regs' = M.insert d v regs
                            in loop ram free valStack ipStack regs' cmp (ip+1)

                    where
                    fromLitInt d i =
                        let regs' = M.insert d i regs
                        in loop ram free valStack ipStack regs' cmp (ip+1)

                    toFrom d r =
                        let Just val = M.lookup r regs
                            regs' = M.insert d val regs
                        in loop ram free valStack ipStack regs' cmp (ip+1)

    loop M.empty 0 [] [] M.empty False (loc iv "main")

    where
    loc iv f = fromJust $ V.findIndex isLabel iv
        where
        isLabel (CLabel l) = l == f
        isLabel          _ = False