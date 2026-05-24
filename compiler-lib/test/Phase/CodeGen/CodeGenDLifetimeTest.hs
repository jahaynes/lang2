{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGenDLifetimeTest (codeGenDLifetimeTests) where

import Phase.CodeGen.CodeGenD
import Phase.CodeGen.TypesD

import           Hedgehog

codeGenDLifetimeTests :: Group
codeGenDLifetimeTests =
    Group "CodeGenD Lifetime Analysis"
        [ ("empty_list",              test_emptyList)
        , ("single_def_no_use",       test_singleDefNoUse)
        , ("single_def_then_use",     test_singleDefThenUse)
        , ("def_use_def_use",         test_defUseDefUse)
        , ("live_in_register",        test_liveInRegister)
        , ("dneg_reads_then_writes",  test_dnegReadsThenWrites)
        , ("dbin_reads_two_regs",     test_dbinReadsTwoRegs)
        , ("multiple_intervals",      test_multipleIntervals)
        , ("dfun_free_vars",          test_dfunFreeVars)
        , ("dcall_callreg",          test_dcallCallReg)
        , ("dret_register",          test_dretRegister)
        ]

unitTest :: PropertyT IO () -> Property
unitTest p = withTests 1 $ property p

test_emptyList :: Property
test_emptyList =
    unitTest $ do
        fst (computeLifetimes []) === []

test_singleDefNoUse :: Property
test_singleDefNoUse =
    unitTest $
        let instrs = [DMov (FromLitInt (R 0) 42)]
            (intervals, _) = computeLifetimes instrs
        in intervals === [LiveInterval (R 0) 0 0]

test_singleDefThenUse :: Property
test_singleDefThenUse =
    unitTest $
        let r0 = R 0
            instrs = [ DMov (FromLitInt r0 42)
                     , DPush (DReg r0)
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [LiveInterval r0 0 1]

test_defUseDefUse :: Property
test_defUseDefUse =
    unitTest $
        let r0 = R 0
            instrs = [ DMov (FromLitInt r0 1)    -- def r0 at 0
                     , DPush (DReg r0)            -- use r0 at 1
                     , DMov (FromLitInt r0 2)    -- def r0 at 2 (new interval)
                     , DPush (DReg r0)            -- use r0 at 3
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [ LiveInterval r0 0 1
                         , LiveInterval r0 2 3
                         ]

test_liveInRegister :: Property
test_liveInRegister =
    unitTest $
        let r0 = R 0
            instrs = [ DPush (DReg r0)   -- use r0 at 0 (before any def -> live-in)
                     , DPop r0           -- def r0 at 1
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [ LiveInterval r0 0 0   -- live-in interval [0,0]
                         , LiveInterval r0 1 1   -- def-use interval [1,1]
                         ]

test_dnegReadsThenWrites :: Property
test_dnegReadsThenWrites =
    unitTest $
        let r0 = R 0
            instrs = [ DMov (FromLitInt r0 10)   -- def r0 at 0
                     , DNeg r0                    -- use+def r0 at 1
                     , DPush (DReg r0)            -- use r0 at 2
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [ LiveInterval r0 0 1   -- first interval: def at 0, last use at 1 (the read by DNeg)
                         , LiveInterval r0 1 2   -- second interval: def at 1 (DNeg write), last use at 2
                         ]

test_dbinReadsTwoRegs :: Property
test_dbinReadsTwoRegs =
    unitTest $
        let ra = R 0
            rb = R 1
            rd = R 2
            instrs = [ DMov (FromLitInt ra 3)   -- def ra at 0
                     , DMov (FromLitInt rb 4)   -- def rb at 1
                     , DBin rd DPlus (DReg ra) (DReg rb)  -- use ra,rb at 2, def rd at 2
                     , DPush (DReg rd)          -- use rd at 3
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [ LiveInterval ra 0 2
                         , LiveInterval rb 1 2
                         , LiveInterval rd 2 3
                         ]

test_multipleIntervals :: Property
test_multipleIntervals =
    unitTest $
        let ra = R 0
            rb = R 1
            instrs = [ DMov (FromLitInt ra 1)    -- def ra at 0
                     , DMov (FromLitInt rb 2)    -- def rb at 1
                     , DBin ra DPlus (DReg ra) (DReg rb)  -- use ra,rb at 2; def ra at 2 (new)
                     , DPush (DReg ra)            -- use ra at 3
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [ LiveInterval ra 0 2   -- first ra interval: def at 0, use at 2
                         , LiveInterval ra 2 3   -- second ra interval: def at 2, use at 3
                         , LiveInterval rb 1 2   -- rb interval: def at 1, use at 2
                         ]

test_dfunFreeVars :: Property
test_dfunFreeVars =
    unitTest $ do
        let r0 = R 0
            r1 = R 1
            inner = [ DMov (FromLitInt r0 10) ]  -- inner body uses its own r0
            instrs = [ DMov (FromLitInt r0 5)    -- def r0 at 0 (outer)
                     , DMov (FromLitInt r1 6)    -- def r1 at 1 (outer)
                     , DFun [DReg r0, DReg r1] inner  -- use r0,r1 at 2 (outer); inner has own intervals
                     ]
            (intervals, innerIntervals) = computeLifetimes instrs
        intervals === [ LiveInterval r0 0 2
                      , LiveInterval r1 1 2
                      ]
        innerIntervals === [[LiveInterval r0 0 0]]

-- | A register defined, then used via 'DCall (CallReg r)', then used again.
test_dcallCallReg :: Property
test_dcallCallReg =
    unitTest $
        let r0 = R 0
            instrs = [ DMov (FromLitInt r0 7)    -- def r0 at 0
                     , DCall (CallReg r0)        -- use r0 at 1
                     , DPush (DReg r0)           -- use r0 at 2
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [LiveInterval r0 0 2]

-- | A register defined, used, and then used as the return value via 'DRet'.
test_dretRegister :: Property
test_dretRegister =
    unitTest $
        let r0 = R 0
            instrs = [ DMov (FromLitInt r0 42)   -- def r0 at 0
                     , DPush (DReg r0)           -- use r0 at 1
                     , DRet (DReg r0)            -- use r0 at 2 (last use)
                     ]
            (intervals, _) = computeLifetimes instrs
        in intervals === [LiveInterval r0 0 2]