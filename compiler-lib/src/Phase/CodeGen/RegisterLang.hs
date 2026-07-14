{-# LANGUAGE DeriveFunctor #-}

module Phase.CodeGen.RegisterLang where

import Core.Module  (DataDefn, Quant)
import Core.Operator (BinOp, UnOp)
import Core.Types    (Type)

-- | Virtual register
newtype R = R Int
    deriving (Eq, Ord, Show)

-- | Basic block label
newtype L = L Int
    deriving (Eq, Ord, Show)

---------------------------------------------------------------
-- Program structure
---------------------------------------------------------------

-- | A complete register-module: data definitions + function definitions
data RegModule s =
    RegModule { getRegDataDefns :: [DataDefn s]
              , getRegFunDefs   :: [RegFunDef s]
              } deriving Show

-- | A register-level function definition:
--   name, quantifier, result-type, env-var-count, param-count, blocks
data RegFunDef s =
    RegFunDef s (Quant s) (Type s) !Int !Int [Block s]
        deriving Show

-- | A basic block: label, instructions, terminator
data Block s =
    Block L [Inst s] (Terminator s)
        deriving Show

---------------------------------------------------------------
-- Instructions
---------------------------------------------------------------

data Inst s = Move     !(Type s) !R !(ATerm s)   -- dest ← atomic value
            | UnOp     !(Type s) !R !UnOp !R      -- dest ← unary op (src)
            | BinOp    !(Type s) !R !BinOp !R !R  -- dest ← binop (src1, src2)
            | Alloc    !(Type s) !R !s [R]        -- dest ← allocate data-con with fields
            | Proj     !(Type s) !R !Int !R       -- dest ← src[i] (field projection)
            | LoadEnv  !(Type s) !R !Int          -- dest ← env[i]
            | AllocClos !(Type s) !R !L [R]       -- dest ← closure(func-label, captured regs)
              deriving Show

-- | Atomic terms (representable in a single register load)
data ATerm s = AVar R
             | ADCons s
             | ALitInt  !Integer
             | ALitBool !Bool
             | ALitString s
               deriving Show

---------------------------------------------------------------
-- Block terminators
---------------------------------------------------------------

data Terminator s = Return    !(Type s) [R]
                  | Jump      !L
                  | Branch    !(Type s) !R !L !L
                  | Case      !(Type s) !R [(PPat s, L)] !L
                  | Call      !(Type s) [R] !s [R] !L
                  | CallClos  !(Type s) [R] !R !R [R] !L
                    deriving Show

---------------------------------------------------------------
-- Patterns for case matching
---------------------------------------------------------------

data PPat s = PVar s
            | PApp s !(Type s) [PPat s]
              deriving (Functor, Show)

---------------------------------------------------------------
-- Old names (kept for backwards compatibility)
---------------------------------------------------------------

data DBinOp = Plus
            | Minus
            | Times
            | Div
            | Mod
            | Eq
            | And
            | Or
            | Lt
              deriving (Eq, Ord, Show)
