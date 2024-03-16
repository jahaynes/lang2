module Phase.CodeGen.TypesC where

import Data.Text (Text)

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

data CallDest s = CallLabel !s
                | CallReg !Int
                | CallClosureAddr !Int

data CVal s = CLitInt !Int
            | CReg !Int
            | CLbl !s

data MovMode s = MovMode
               | ToFrom !Int !(CVal s)
               | ToOffsetFrom !Int !Int !(CVal s)
               | ToFromOffset !Int !Int !Int

typesCMain :: IO ()
typesCMain = do


    putStrLn "hu"


mainIs :: [CInstr Text]
mainIs = [ CPush (CLitInt 1)
         , CCall (CallLabel "f")
         , CPop 1 -- reg a
         , CPush (CLitInt 2)
         , CCall (CallClosureAddr 1 {- reg a -})
         , CPop 2 -- reg b
         , CRet (CReg 2 {- reg b -})
         ]

fIs :: [CInstr Text]
fIs = [ CPop 3 {- r3 <- pop x -}
      , CTimes 4 (CReg 3) (CReg 3) {- r4 <- r3 * r3 -}
      , CAlloc 5 16
      , CMov (ToFrom 5 (CLbl "anf_0"))
      , CMov (ToOffsetFrom 5 8 (CReg 4))
      , CRet (CReg 5)
      ]

anf0Is :: [CInstr s]
anf0Is = [ CPop 6 {-pop &env-}
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


