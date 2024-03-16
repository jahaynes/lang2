module Phase.CodeGen.TypesC where

import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Text (Text, unpack)
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Debug.Trace (trace)

data CInstr s = CComment !s 
              | CLabel !s

              | CCall !(CallDest s)
              | CRet !(CVal s)

              | CPush !(CVal s)
              | CPop !Int

              | CAlloc !Int !Int -- destreg sz

              | CPlus !Int !(CVal s) !(CVal s) -- dest reg / a / b 
              | CTimes !Int !(CVal s) !(CVal s) -- dest reg / a / b 

              | CMov !(MovMode s)
                  deriving Show

data CallDest s = CallLabel !s
                | CallReg !Int
                | CallClosureAddr !Int  -- reg containing ptr to closure
                    deriving Show

data CVal s = CLitInt !Int
            | CReg !Int
            | CLbl !s
                deriving Show

data MovMode s = MovMode
               | ToOffsetFrom !Int !Int !(CVal s)
               | ToFromOffset !Int !Int !Int
                   deriving Show

typesCMain :: IO ()
typesCMain = do

    mapM_ print $ zip [0..] allInstrs
    putStrLn ""

    interpret allInstrs



interpret :: [CInstr Text] -> IO ()
interpret is = do
    
    let iv = V.fromList is

    let loop !ram !free !valStack !ipStack !regs !ip =
            let i = iv ! ip in
            case i of

                CLabel{} -> trace ("At label, ipstack: " ++ show ipStack) $
                    loop ram free valStack ipStack regs (ip+1)

                CPush (CLitInt i) ->
                    let valStack' = i:valStack
                    in loop ram free valStack' ipStack regs (ip+1)

                CCall (CallLabel f) -> trace ("calling " ++ unpack f) $
                    let ipStack' = ip+1:ipStack
                    in loop ram free valStack ipStack' regs (loc iv f)

                CCall (CallClosureAddr r) -> trace ("calling closure") $
                    let Just p   = M.lookup r regs
                        Just v   = M.lookup p ram
                        ipStack' = ip+1:ipStack
                    in loop ram free valStack ipStack' regs v

                CPop r ->
                    let (v:valStack') = valStack
                        regs' = M.insert r v regs
                    in loop ram free valStack' ipStack regs' (ip+1)

                CTimes d a b ->
                    let Just a' = case a of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        Just b' = case b of
                                      CReg r    -> M.lookup r regs
                                      CLitInt i -> Just i
                        regs' = M.insert d (a' * b') regs
                    in loop ram free valStack ipStack regs' (ip+1)

                CAlloc r sz ->
                    let (cells, 0) = sz `divMod` 8
                        free' = free + cells
                        regs' = M.insert r free regs
                    in loop ram free' valStack ipStack regs' (ip+1)

                CMov (ToOffsetFrom d o (CLbl f)) ->
                    let Just p    = M.lookup d regs
                        (cell, 0) = (p + o) `divMod` 8
                        ram' = M.insert cell (loc iv f) ram
                    in loop ram' free valStack ipStack regs (ip+1)

                CMov (ToOffsetFrom d o (CReg r)) ->
                    let Just v    = M.lookup r regs
                        Just p    = M.lookup d regs
                        (cell, 0) = (p + o) `divMod` 8
                        ram' = M.insert cell v ram
                    in loop ram' free valStack ipStack regs (ip+1)
    
                CMov (ToFromOffset d r o) ->
                    let Just v    = M.lookup r regs
                    in error $ show v

                CRet (CReg r) -> trace ("returning, ipstack: " ++ show ipStack) $
                    let Just v = M.lookup r regs
                    in case ipStack of
                           [] -> do
                               print ram
                               print v
                           (ip':stack') ->
                               let valStack' = v:valStack
                               in loop ram free valStack' stack' regs ip'



                _        -> error $ "Unhandled " ++ show i

    loop M.empty 0 [] [] M.empty 0

    where
    loc iv f = fromJust $ V.findIndex isLabel iv
        where
        isLabel (CLabel l) = l == f
        isLabel          _ = False

allInstrs = concat [mainIs, fIs, anf0Is]



mainIs :: [CInstr Text]
mainIs = [ CLabel "main"
         , CPush (CLitInt 1)
         , CCall (CallLabel "f")
         , CPop 1 -- reg a
         , CPush (CLitInt 2)
         , CCall (CallClosureAddr 1 {- reg a -})
         , CPop 2 -- reg b
         , CRet (CReg 2 {- reg b -})
         ]

fIs :: [CInstr Text]
fIs = [ CLabel "f"
      , CPop 3 {- r3 <- pop x -}
      , CTimes 4 (CReg 3) (CReg 3) {- r4 <- r3 * r3 -}
      , CAlloc 5 16
      , CMov (ToOffsetFrom 5 0 (CLbl "anf_0"))
      , CMov (ToOffsetFrom 5 8 (CReg 4))
      , CRet (CReg 5)
      ]

anf0Is :: [CInstr Text]
anf0Is = [ CLabel "anf_0"
         , CPop 6 {-pop &env-}
         , CMov (ToFromOffset 7 6 8) {- xx = r6[+8] -}
         , CPop 8 {-pop y-}
         , CPlus 9 (CReg 8) (CReg 7)
         , CRet (CReg 9) 
         ]

{-

-----------------------------------

f x =
  let xx = x * x in
  (\y. y + xx)

main = (f 1) 2

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


