module Phase.CodeGen.TypesC where

import Common.State

import           Control.Monad (zipWithM_)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Text (Text, unpack)
import           Data.Vector ((!))
import qualified Data.Vector as V
import Debug.Trace (trace)
import Text.Printf (printf)
import System.IO

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
                  deriving Show

data CallDest s = CallLabel !s
                | CallReg !R
                | CallClosureAddr !R  -- reg containing ptr to closure
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

typesCMain :: IO ()
typesCMain = do

    hSetBuffering stdout LineBuffering

    zipWithM_ render [0..] allInstrs
    putStrLn ""

    interpret allInstrs

    where
    render :: Int -> CInstr Text -> IO ()
    render lineNo c@CLabel{} = do
        putStr (show lineNo ++ " ")
        print c
    render lineNo c = do
        putStr (show lineNo)
        putStr "\t"
        print c

interpret :: [CInstr Text] -> IO ()
interpret is = do
    
    let iv = V.fromList is

    let loop !ram !free !valStack !ipStack !regs !ip = do
            let i = iv ! ip
            putStr $ show ip ++ "\t"
            case i of

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
                    loop ram free' valStack ipStack regs' (ip+1)

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
                        [] -> printf "No IpStack. %s = %s\n" (show r) (show $ M.lookup r regs)
                        (ip':stack') -> do
                            let valStack' = v:valStack
                            printf "pushed %d and returning to %d\n" v ip'
                            loop ram free valStack' stack' regs ip'

                _        -> error $ "Unhandled " ++ show i

    loop M.empty 0 [] [] M.empty (13 {- TODO -} )

    where
    loc iv f = fromJust $ V.findIndex isLabel iv
        where
        isLabel (CLabel l) = l == f
        isLabel          _ = False

allInstrs :: [CInstr Text]
allInstrs = evalState go (R 0)
    where
    go = do
        a <- fInstrs
        b <- anf0Instr
        c <- mainInstr
        pure $ concat [a, b, c]

fInstrs :: State R [CInstr Text]
fInstrs = do
    -- Args
    x <- freshR
    let pop = CPop x
    -- Some body
    xx <- freshR
    let mul = CTimes xx (CReg x) (CReg x)
    -- Alloc the closure
    clo <- freshR
    let alloc = CAlloc clo 16
    let copyInFPtr = CMov $ ToOffsetFrom clo 0 (CLbl "anf_0")
    let copyInXx   = CMov $ ToOffsetFrom clo 8 (CReg xx)
    let ret = CRet (CReg clo)
    -- clo holds a pointer
    pure [CLabel "f", pop, mul, alloc, copyInFPtr, copyInXx, ret]

anf0Instr :: State R [CInstr Text]
anf0Instr = do

    -- Env { xx }
    env <- freshR
    let popEnv = CPop env
    xx <- freshR
    let readXx = CMov $ ToFromOffset xx env 8

    -- Format params
    y <- freshR
    let popY = CPop y
    yxx <- freshR

    let plus = CPlus yxx (CReg y) (CReg xx)
    let ret  = CRet (CReg yxx)

    pure [CLabel "anf_0", popEnv, readXx, popY, plus, ret]

freshR :: State R R
freshR = do
    r@(R i) <- get
    put . R $ i + 1
    pure r

mainInstr :: State R [CInstr Text]
mainInstr = do

    clo <- freshR
    let pushCallPop = [ CPush (CLitInt 3)
                      , CCall (CallLabel "f")
                      , CPop clo ]

    ret <- freshR
    let cloCall = [ CPush (CLitInt 4) -- (rev order)
                  , CPush (CReg clo) -- push the closure itself (Acting as the env)
                  , CCall $ CallClosureAddr clo
                  , CPop ret
                  , CRet $ CReg ret ]

    pure $ concat [[CLabel "main"], pushCallPop, cloCall]

{-

-----------------------------------

f x =
  let xx = x * x in
  (\y. y + xx)

main = (f 3) 4

-----------------------------------

anf_0 {_, xx} y =
    y + xx

f x =
    let xx = x * x
    let clo = alloc {anf_0, xx}
    (&)clo

main =
    
    push 1
    call (Lbl f)
    a <- pop :: Int -> Int
    push 2
    call (cloaddr a)
    b <- pop :: Int
    ret b

-}


