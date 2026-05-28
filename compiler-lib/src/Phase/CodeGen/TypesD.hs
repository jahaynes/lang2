{-# LANGUAGE QuasiQuotes #-}

module Phase.CodeGen.TypesD ( CallDest (..)
                            , DBinOp (..)
                            , DInstr (..)
                            , DVal (..)
                            , MovMode (..)
                            , R (R)
                            ) where

import Data.String.Interpolate (i)

newtype R =
    R Int
        deriving (Eq, Ord)

instance Show R where
    show (R n) = 'r':show n

data DInstr s = DComment !s 
              | DLabel !s

              | DPush !(DVal s)

              | DPop !R

              | DCall !(CallDest s)
                
          --    | DFun ![DVal s] [DInstr s]

              | DRet !(DVal s)

              --    Dest Op A B
              | DBin !R !DBinOp !(DVal s) !(DVal s)

              | DNeg !R -- negate a register

              | DCmpB !R
              | J !s
         --     | Je !s
              | Jne !s

              | DMov !(MovMode s)
              | DErr !s

--                  deriving Show

-- Hacky newtype for stripping excess quotes
newtype Uq s =
    Uq s

instance Show s => Show (Uq s) where
    show (Uq s) = init . tail . show $ s

instance Show s => Show (DInstr s) where

    show (DComment c) = [i|// #{c}|]

    show (DLabel l) = [i|#{Uq l}:|]

    show (DPush v) = [i|push #{v}|]

    show (DPop r) = [i|#{r} <- pop|]

    show (DCall dst) = [i|call #{dst}|]

    show (DRet v) = [i|ret #{v}|]

    show (DBin dst op a b) = [i|#{dst} <- #{a} #{op} #{b}|]

    show (DNeg r) = [i|-#{r}|]

    show (DCmpB r) = [i|cmp #{r}|]

    show (J l) = [i|jmp #{l}|]

    show (Jne l) = [i|jmpNe #{l}|]

    show (DMov mode) = show mode

data DBinOp = DPlus
            | DMinus
            | DTimes
            | DDiv
            | DMod 
            | DEq
            | DAnd
            | DOr
            | DLt

instance Show DBinOp where
    show op =
        case op of
            DPlus  -> "+"
            DMinus -> "-"
            DTimes -> "*"
            DDiv   -> "/"
            DMod   -> "%"
            DEq    -> "=="
            DAnd   -> "&&"
            DOr    -> "||"
            DLt    -> "<"

data CallDest s = CallLabel !s
                | CallReg !R                                      
                    deriving Show

data DVal s = DLitInt !Int
            | DReg !R
            | DLbl !s

instance Show s => Show (DVal s) where
    show (DLitInt i) = show i
    show (DReg r)    = show r
    show (DLbl s)    = show (Uq s)

data MovMode s = ToFrom !R !R
               | FromLitInt !R !Int
               | ToOffsetFrom !R !Int !(DVal s)
               | ToFromOffset !R !R !Int -- 2nd reg is a pointer
                   deriving Show
