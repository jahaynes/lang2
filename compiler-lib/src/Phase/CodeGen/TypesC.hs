module Phase.CodeGen.TypesC where

newtype R =
    R Int
        deriving (Eq, Ord)

instance Show R where
    show (R n) = 'r':show n

-- newtype Val, Ptr, Off, Sz

data CInstr s = CComment !s 
              | CLabel !s

              | CCall !(CallDest s)
              | CRet !(CVal s)

              | CPush !(CVal s)
              | CPop !R

              | CAlloc !R !Int -- destreg sz

              | CPlus  !R !(CVal s) !(CVal s) -- dest reg / a / b
              | CMinus !R !(CVal s) !(CVal s) -- dest reg / a / b
              | CTimes !R !(CVal s) !(CVal s) -- dest reg / a / b

              | CEq  !R !(CVal s) !(CVal s) -- dest reg / a / b
              | CAnd !R !(CVal s) !(CVal s) -- dest reg / a / b

              | CLt !R !(CVal s) !(CVal s) -- dest reg / a / b

              | CCmpB !R
              | J !s
              | Jne !s

              | CMov !(MovMode s)
                  deriving Show

data CallDest s = CallLabel !s
                | CallReg !R
                | CallClosureAddr !R  -- reg containing ptr to closure... same as callreg?
                    deriving Show

data CVal s = CLitInt !Int
            | CReg !R
            | CLbl !s
                deriving Show

data MovMode s = ToFrom !R !R
               | FromLitInt !R !Int
               | ToOffsetFrom !R !Int !(CVal s)
               | ToFromOffset !R !R !Int            -- 2nd reg is a pointer
                   deriving Show
