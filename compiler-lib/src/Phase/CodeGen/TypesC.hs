{-# LANGUAGE DeriveFunctor, Strict #-}
module Phase.CodeGen.TypesC where

import Common.State

import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Text (Text, unpack)
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Text.Printf (printf)
import           System.IO

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

              | CPlus !R !(CVal s) !(CVal s) -- dest reg / a / b
              | CTimes !R !(CVal s) !(CVal s) -- dest reg / a / b

              | CMov !(MovMode s)
                  deriving (Functor, Show)

data CallDest s = CallLabel !s
                | CallReg !R
                | CallClosureAddr !R  -- reg containing ptr to closure... same as callreg?
                    deriving (Functor, Show)

data CVal s = CLitInt !Int
            | CReg !R
            | CLbl !s
                deriving (Functor, Show)

data MovMode s = ToFrom !R !R
               | FromLitInt !R !Int
               | ToOffsetFrom !R !Int !(CVal s)
               | ToFromOffset !R !R !Int            -- 2nd reg is a pointer
                   deriving (Functor, Show)

interpret :: [CInstr Text] -> IO Int
interpret is = do
    
    hSetBuffering stdout LineBuffering

    let iv = V.fromList is

    let loop !ram !free !valStack !ipStack !regs !ip = do
            let i = iv ! ip
            putStr $ show ip ++ "\t"
            case i of

                CComment{} ->
                    loop ram free valStack ipStack regs (ip+1)

                CLabel l -> do
                    putStrLn $ unpack l
                    loop ram free valStack ipStack regs (ip+1)

                CPush (CLitInt i) -> do
                    let valStack' = i:valStack
                    printf "pushed literal %d\n" i
                    loop ram free valStack' ipStack regs (ip+1)

                CPush (CReg r) -> do
                    let Just v = M.lookup r regs
                    let valStack' = v:valStack
                    printf "pushed val %d\n" v
                    loop ram free valStack' ipStack regs (ip+1)

                CCall (CallReg r) -> do
                    let ipStack' = ip+1:ipStack
                    let Just ip' = M.lookup r regs
                    printf "Calling from reg to location %d\n" ip'
                    loop ram free valStack ipStack' regs ip'

                CCall (CallLabel f) -> do
                    let ipStack' = ip+1:ipStack
                    printf "Calling %s\n" f
                    loop ram free valStack ipStack' regs (loc iv f)

                CCall (CallClosureAddr r) -> do
                    let Just p   = M.lookup r regs
                        Just v   = M.lookup p ram
                        ipStack' = ip+1:ipStack
                    printf "Jumping to closure at ip %d\n" v
                    loop ram free valStack ipStack' regs v

                CPop r -> do
                    let (v:valStack') = valStack
                        regs' = M.insert r v regs
                    printf "%s <- Pop %d\n" (show r) (v)
                    loop ram free valStack' ipStack regs' (ip+1)

                CPlus d a b -> do
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (a' + b') regs
                    printf "%s = %d + %d\n" (show d) a' b'
                    loop ram free valStack ipStack regs' (ip+1)

                CTimes d a b -> do
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (a' * b') regs
                    printf "%s = %d * %d\n" (show d) a' b'
                    loop ram free valStack ipStack regs' (ip+1)

                CAlloc r sz -> do
                    let (cells, 0) = sz `divMod` 8
                        free' = free + cells
                        regs' = M.insert r free regs
                    printf "%s = alloc %d\n" (show r) sz
                    print ("regs", regs')
                    loop ram free' valStack ipStack regs' (ip+1)

                CMov (ToFrom d r) -> do
                    printf "%s <- %s\n" (show d) (show r)
                    let Just val = M.lookup r regs
                    let regs' = M.insert d val regs
                    loop ram free valStack ipStack regs' (ip+1)

                CMov (ToOffsetFrom d o (CLbl f)) -> do
                    let Just p = M.lookup d regs
                        (cell, 0) = (p + o) `divMod` 8
                        ram' = M.insert cell (loc iv f) ram
                    printf "%s -> %s\n" (show ram) (show ram')
                    loop ram' free valStack ipStack regs (ip+1)

                CMov (ToOffsetFrom d o (CReg r)) -> do
                    let Just p = M.lookup d regs
                        (cell, 0) = (p + o) `divMod` 8
                        Just v = M.lookup r regs
                        ram' = M.insert cell v ram
                    printf "%s -> %s\n" (show ram) (show ram')
                    loop ram' free valStack ipStack regs (ip+1)
    
                CMov (ToFromOffset d r o) -> do
                    let Just p = M.lookup r regs
                        (cell, 0) = (p + o) `divMod` 8
                        Just v = M.lookup cell ram
                        regs' = M.insert d v regs
                    printf "%s <- from ram, val %d\n" (show d) v 
                    loop ram free valStack ipStack regs' (ip+1)

                CRet (CReg r) -> do
                    let Just v = M.lookup r regs
                    case ipStack of
                        [] -> do
                            printf "No IpStack. %s = %s\n" (show r) (show v)
                            pure v
                        (ip':stack') -> do
                            let valStack' = v:valStack
                            printf "pushed %d and returning to %d\n" v ip'
                            loop ram free valStack' stack' regs ip'

                _        -> error $ "Unhandled " ++ show i

    loop M.empty 0 [] [] M.empty (loc iv "main")

    where
    loc iv f = fromJust $ V.findIndex isLabel iv
        where
        isLabel (CLabel l) = l == f
        isLabel          _ = False

freshR :: State R R
freshR = do
    r@(R i) <- get
    put . R $ i + 1
    pure r
