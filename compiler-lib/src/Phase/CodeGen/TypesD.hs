module Phase.CodeGen.TypesD ( CallDest (..)
                            , DBinOp (..)
                            , DInstr (..)
                            , DVal (..)
                            , MovMode (..)
                            , R (R)
                            ) where

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
                
              | DFun ![DVal s] [DInstr s]

              | DRet !(DVal s)

              --    Dest Op A B
              | DBin !R !DBinOp !(DVal s) !(DVal s)

              | DNeg !R -- negate a register

              | DCmpB !R
              | J !s
              | Je !s
              | Jne !s

              | DMov !(MovMode s)
              | DErr !s

                  deriving Show

data DBinOp = DPlus
            | DMinus
            | DTimes
            | DDiv
            | DMod 
            | DEq
            | DAnd
            | DOr
            | DLt
                deriving Show

data CallDest s = CallLabel !s
                | CallReg !R                                      
                    deriving Show

data DVal s = DLitInt !Int
            | DReg !R
            | DLbl !s
                deriving Show

data MovMode s = ToFrom !R !R
               | FromLitInt !R !Int
               | ToOffsetFrom !R !Int !(DVal s)
               | ToFromOffset !R !R !Int -- 2nd reg is a pointer
                   deriving Show
