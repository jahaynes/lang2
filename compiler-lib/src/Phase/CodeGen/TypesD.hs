module Phase.CodeGen.TypesD ( DBinOp (..)
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

              --      Dest Label Args
              | DCall !R !(CallDest s) ![DVal s]
                  {- This is OK for IR, but it's missing the Pops at the function site -}
                
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
                | CallClosureAddr !R  -- reg containing ptr to closure... same as callreg?
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
